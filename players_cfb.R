# =============================================================================
# players_cfb.R -- player ratings and roster features. Sourced by build_cfb.R.
#
# Every non-garbage scrimmage play is split between the people on it:
#   pass play:  EPA = league + passer + pass defense
#   run play:   EPA = league + ball carrier + team run game + run defense
# fitted by ridge regression, one season at a time. Sacks count against the
# passer. The team run term soaks up blocking and scheme, so a back is
# credited only with what the offense's other runs don't explain.
# Receivers are a second step: each target's EPA minus what that passer and
# that defense would produce on an average throw, averaged per receiver and
# shrunk toward zero as if he had PL_K_REC extra average targets. So a
# receiver's number is how much better throws to him go than his
# quarterback's throws in general. That is noisy: split-half reliability
# across 2016-2025 receivers (25+ targets in each half) is only 0.22, which
# is where PL_K_REC = 150 comes from. Most of a receiver's value here comes
# from how often he's targeted.
# Every number is EPA per play above an average player in that role,
# adjusted for the defenses faced and shrunk toward average by sample size.
#
# Offensive linemen and most defenders have no play-level attribution in
# public data, so they aren't rated (defenders get playmaking counts only).
# =============================================================================

PL_VERSION <- "p1.2"
# ridge penalties in plays: a player with this many plays keeps about half
# of his raw effect. Chosen on 2023-2025 by fitting on half of each season's
# games and predicting the other half (see tune_player_lambdas()).
PL_LAMBDA <- c(qb = 150, rush = 120, team_run = 200, def = 200, team_pass = Inf)
# team_pass (c3.0): an optional team pass-game term, the passing counterpart of
# team_run, which would credit a passer only with what his team's other throws
# don't explain. It is OFF (Inf) because it tested worse: across 2014-2025, a
# passer's number predicted his next season's EPA per dropback less well the
# stronger the term, both for the 98 quarterbacks who changed teams (r 0.16 with
# no term, 0.12 at 400, 0.02 at 100) and the 529 who stayed (0.37, 0.33, 0.17).
# Rerun tune_team_pass() to check again as more transfer seasons arrive.
PL_K_REC  <- 150
PL_MIN_DISPLAY <- c(qb = 60, rush = 40, rec = 20)   # plays to show on leaderboards
PL_GAMES_FOR_STARTER <- 2   # live and backtest: a team's QB is its leader in dropbacks over its first N games

mode1 <- function(x) { x <- x[!is.na(x) & nzchar(x)]; if (!length(x)) NA_character_ else names(which.max(table(x))) }

# ---------------------------------------------------------------- the fit
# Missing attribution: cfbfastR names the target on nearly every completion
# but on far fewer incompletions (under 40% in 2022-2024), which would make
# every receiver look better than he is. A pass with no recorded target
# (not a sack) is therefore spread across the team's receivers in proportion
# to their recorded targets that season, and a pass with no recorded passer
# across that game's passers. Shares under 2% are dropped.
share_rows <- function(rows, keys_dt, by) {
  # rows: data.table(r, <by cols>); keys_dt: data.table(<by cols>, id, w)
  m <- merge(rows, keys_dt, by = by, allow.cartesian = TRUE)
  m[, .(r, id, w)]
}

fit_players <- function(plays, lam = PL_LAMBDA, train_games = NULL) {
  pl <- if (is.null(train_games)) plays else plays[game_id %in% train_games]
  ps <- pl[is_pass == TRUE]; ru <- pl[is_rush == TRUE]
  np <- nrow(ps); nr <- nrow(ru)
  ps[, r := .I]; ru[, r := np + .I]
  # passer weights
  qb_known <- ps[!is.na(qb_id), .(r, id = qb_id, w = 1)]
  qsh <- ps[!is.na(qb_id), .N, by = .(game_id, off_id, id = qb_id)][, w := N / sum(N), by = .(game_id, off_id)][w >= 0.02]
  qsh[, w := w / sum(w), by = .(game_id, off_id)]   # renormalise after dropping small shares (as rsh does)
  qb_imp <- share_rows(ps[is.na(qb_id), .(r, game_id, off_id)], qsh[, .(game_id, off_id, id, w)], c("game_id", "off_id"))
  qbw <- rbind(qb_known, qb_imp)
  # target weights
  rec_known <- ps[!is.na(wr_id), .(r, id = wr_id, w = 1)]
  rsh <- ps[!is.na(wr_id), .N, by = .(off_id, id = wr_id)][, w := N / sum(N), by = off_id][w >= 0.02]
  rsh[, w := w / sum(w), by = off_id]
  rec_imp <- share_rows(ps[is.na(wr_id) & sack == FALSE, .(r, off_id)], rsh[, .(off_id, id, w)], "off_id")
  recw <- rbind(rec_known, rec_imp)
  use_tp <- is.finite(lam[["team_pass"]] %||% Inf)
  keys <- list(
    qb = sort(unique(qbw$id)), team_pass = if (use_tp) sort(unique(ps$off_id)) else integer(),
    rush = sort(unique(ru$rb_id[!is.na(ru$rb_id)])), team_run = sort(unique(ru$off_id)),
    defp = sort(unique(ps$def_id)), defr = sort(unique(ru$def_id)))
  base <- list(); cur <- 2L
  for (k in names(keys)) { base[[k]] <- cur; cur <- cur + length(keys[[k]]) }
  p <- cur
  col <- function(k, v) base[[k]] + match(v, keys[[k]])
  rb_ok <- !is.na(ru$rb_id)
  i <- c(ps$r, ps$r, qbw$r, ru$r, ru$r[rb_ok], ru$r, ru$r, if (use_tp) ps$r)
  j <- c(rep(1L, np), col("defp", ps$def_id), col("qb", qbw$id),
         rep(2L, nr), col("rush", ru$rb_id[rb_ok]), col("team_run", ru$off_id), col("defr", ru$def_id),
         if (use_tp) col("team_pass", ps$off_id))
  v <- c(rep(1, np), rep(1, np), qbw$w, rep(1, nr), rep(1, sum(rb_ok)), rep(1, nr), rep(1, nr), if (use_tp) rep(1, np))
  X <- Matrix::sparseMatrix(i = i, j = j, x = v, dims = c(np + nr, p))
  y <- c(ps$EPA, ru$EPA)
  A <- Matrix::crossprod(X)
  pen <- c(1e-4, 1e-4, rep(lam["qb"], length(keys$qb)), rep(lam[["team_pass"]], length(keys$team_pass)),
           rep(lam["rush"], length(keys$rush)), rep(lam["team_run"], length(keys$team_run)),
           rep(lam["def"], length(keys$defp) + length(keys$defr)))
  A <- A + Matrix::Diagonal(p, x = pen)
  beta <- as.vector(Matrix::solve(A, Matrix::crossprod(X, y)))
  get <- function(k) data.table(id = keys[[k]], eff = beta[base[[k]] + seq_along(keys[[k]])])
  fit <- as.vector(X %*% beta)

  # receivers: what's left of each throw after passer and defense, per receiver
  res <- data.table(r = ps$r, resid = ps$EPA - fit[ps$r])
  rw <- merge(recw, res, by = "r")
  rr <- rw[, .(num = sum(w * resid), den = sum(w)), by = id]
  rec <- rr[, .(id, eff = num / (den + PL_K_REC))]
  list(mu_pass = beta[1], mu_rush = beta[2], qb = get("qb"), rec = rec, rush = get("rush"),
       team_run = get("team_run"), team_pass = get("team_pass"), defp = get("defp"), defr = get("defr"),
       n_qb = qbw[, .(n_cred = sum(w)), by = id], n_rec = rr[, .(id, n_cred = den)])
}

predict_plays <- function(fit, plays) {
  lk <- function(tab, ids) { v <- tab$eff[match(ids, tab$id)]; v[is.na(v)] <- 0; v }
  fifelse(plays$is_pass,
          fit$mu_pass + lk(fit$qb, plays$qb_id) + lk(fit$team_pass, plays$off_id) + lk(fit$defp, plays$def_id),
          fit$mu_rush + lk(fit$rush, plays$rb_id) + lk(fit$team_run, plays$off_id) + lk(fit$defr, plays$def_id))
}

# Split-half check used to choose PL_LAMBDA (run by hand; not part of the build).
tune_player_lambdas <- function(plays_list, grid = list(qb = c(75, 150, 300, 600), rush = c(60, 120, 240),
                                                         team_run = c(100, 200, 400), def = c(100, 200, 400))) {
  lam <- PL_LAMBDA
  score <- function(l) mean(sapply(plays_list, function(pl) {
    g <- unique(pl$game_id); tr <- g[seq_along(g) %% 2 == 1]
    f <- fit_players(pl, l, tr); te <- pl[!(game_id %in% tr)]
    mean((te$EPA - predict_plays(f, te))^2)
  }))
  for (k in names(grid)) {
    s <- sapply(grid[[k]], function(v) { l <- lam; l[k] <- v; score(l) })
    lam[k] <- grid[[k]][which.min(s)]
    message(k, ": ", paste(sprintf("%g=%.5f", grid[[k]], s), collapse = "  "))
  }
  lam
}

# Chooses the team_pass penalty (run by hand; not part of the build). For each
# candidate, fits every season, then asks how well a quarterback's effect predicts
# his raw EPA per dropback the next season, separately for quarterbacks who changed
# teams (where the old team's context doesn't follow him) and those who stayed.
tune_team_pass <- function(plays_by_season, grid = c(Inf, 800, 400, 200, 100), min_n = 150) {
  rows <- list()
  for (tp in grid) {
    lam <- PL_LAMBDA; lam[["team_pass"]] <- tp
    q <- rbindlist(lapply(names(plays_by_season), function(s) {
      pl <- plays_by_season[[s]]
      f <- fit_players(pl, lam)
      raw <- pl[is_pass == TRUE & !is.na(qb_id), .(raw = mean(EPA), n = .N, team = as.integer(names(which.max(table(off_id))))), by = .(id = qb_id)]
      merge(raw, f$qb, by = "id")[, season := as.integer(s)]
    }))
    nxt <- copy(q)[, season := season - 1L]
    d <- merge(q[n >= min_n, .(id, season, eff, team)], nxt[n >= min_n, .(id, season, raw2 = raw, n2 = n, team2 = team)],
               by = c("id", "season"))
    d[, moved := team != team2]
    for (mv in c(TRUE, FALSE)) {
      dd <- d[moved == mv]
      rows[[length(rows) + 1]] <- data.table(team_pass = tp, moved = mv, n = nrow(dd),
                                             r = cov.wt(cbind(dd$eff, dd$raw2), wt = dd$n2, cor = TRUE)$cor[1, 2])
    }
  }
  rbindlist(rows)
}

# ------------------------------------------------------------ season table
# One row per player per role for a season: effect, plays, games, value per game.
player_season <- function(sd, season) {
  pl <- sd$plays
  if (is.null(pl) || !nrow(pl)) return(NULL)
  f <- fit_players(pl)
  gm <- function(dt, idcol, namecol, poscol, role) {
    d <- dt[!is.na(get(idcol)), .(n = .N, raw_epa = mean(EPA), sr = mean(success), games = uniqueN(game_id),
                                  name = mode1(get(namecol)), pos = mode1(get(poscol)),
                                  team = as.integer(names(which.max(table(off_id))))), by = c(idcol)]
    setnames(d, idcol, "id")
    d[, role := role]
    d
  }
  ps <- pl[is_pass == TRUE]; ru <- pl[is_rush == TRUE]
  out <- rbind(gm(ps, "qb_id", "qb", "qb_pos", "qb"), gm(ru, "rb_id", "rb", "rb_pos", "rush"),
               gm(ps, "wr_id", "wr", "wr_pos", "rec"))
  eff <- rbind(f$qb[, role := "qb"], f$rush[, role := "rush"], f$rec[, role := "rec"])
  out <- merge(out, eff, by = c("id", "role"), all.x = TRUE)
  cred <- rbind(f$n_qb[, role := "qb"], f$n_rec[, role := "rec"])
  out <- merge(out, cred, by = c("id", "role"), all.x = TRUE)
  out[is.na(n_cred), n_cred := n]
  # value per game uses credited plays: recorded ones plus shares of unrecorded ones
  out[, `:=`(season = season, value_pg = eff * n_cred / pmax(games, 1))]
  tr <- f$team_run[, .(team = id, run_block = eff, season = season)]
  if (nrow(f$team_pass)) tr <- merge(tr, f$team_pass[, .(team = id, pass_context = eff)], by = "team", all = TRUE)
  else tr[, pass_context := NA_real_]
  tr[, season := season]
  list(players = out[], team_run = tr)
}

# Combined per-player season line (all roles), with a primary position.
player_totals <- function(ps) {
  ps[, .(name = name[which.max(n)], team = team[which.max(n)], pos = mode1(rep(pos, n)),
         value_pg = sum(value_pg, na.rm = TRUE), plays = sum(n), games = max(games)), by = .(season, id)]
}

# -------------------------------------------------------- projections
# Next season's per-play effect from the last two, by role, fitted on the
# history (weighted by plays). Also the average first season for players
# with no earlier FBS plays, by role.
fit_player_projection <- function(hist) {
  m <- list()
  for (r in c("qb", "rush", "rec")) {
    h <- hist[role == r, .(season, id, eff, n)]
    p1 <- copy(h)[, season := season + 1L]; setnames(p1, c("eff", "n"), c("e1", "n1"))
    p2 <- copy(h)[, season := season + 2L]; setnames(p2, c("eff", "n"), c("e2", "n2"))
    d <- merge(h, p1, by = c("season", "id"))
    d <- merge(d, p2, by = c("season", "id"), all.x = TRUE)
    d[is.na(e2), `:=`(e2 = 0, n2 = 0)]
    fit <- lm(eff ~ e1 + e2, data = d, weights = pmin(n, 300))
    new <- hist[role == r][!(paste(id, season) %in% paste(p1$id, p1$season))]
    new <- new[season > min(hist$season)]
    m[[r]] <- list(coef = coef(fit), n = nrow(d), r = cor(d$eff, predict(fit, d)),
                   newcomer = weighted.mean(new$eff, pmin(new$n, 300)))
  }
  m
}

project_players <- function(hist, target_season, pm) {
  out <- list()
  for (r in c("qb", "rush", "rec")) {
    h1 <- hist[role == r & season == target_season - 1L, .(id, e1 = eff, n1 = n, g1 = games, team1 = team, name, pos)]
    h2 <- hist[role == r & season == target_season - 2L, .(id, e2 = eff)]
    d <- merge(h1, h2, by = "id", all.x = TRUE)
    d[is.na(e2), e2 := 0]
    cf <- pm[[r]]$coef
    d[, `:=`(proj = cf[1] + cf["e1"] * e1 + cf["e2"] * e2, use_pg = n1 / pmax(g1, 1), role = r)]
    out[[r]] <- d
  }
  rbindlist(out, fill = TRUE)
}

# Roster features for every team in a season. roster: data.table(team, id)
# of players expected to play; qb_pick: data.table(team, id) of each team's
# starting QB where known (else the roster QB with the best projection).
roster_features <- function(roster, qb_pick, proj, pm, season) {
  pq <- proj[role == "qb"]
  qb <- merge(qb_pick, pq[, .(id, proj, n1)], by = "id", all.x = TRUE)
  # teams without a named starter: best projection among roster QBs with 50+ dropbacks
  missing_t <- setdiff(unique(roster$team), qb$team)
  if (length(missing_t)) {
    cand <- merge(roster[team %in% missing_t], pq[n1 >= 50, .(id, proj, n1)], by = "id")
    cand <- cand[order(-proj)][, .SD[1], by = team]
    qb <- rbind(qb, cand[, .(id, team, proj, n1)], fill = TRUE)
  }
  qb[, qb_new := is.na(proj)]
  qb[is.na(proj), proj := pm$qb$newcomer]
  qb <- qb[, .(qb_proj = proj[1], qb_new = qb_new[1]), by = team]
  sk <- merge(roster, proj[role %in% c("rush", "rec"), .(id, role, proj, use_pg)], by = "id")
  sk <- sk[, .(skill_proj = sum(proj * pmin(use_pg, 25))), by = team]
  f <- merge(data.table(team = unique(roster$team)), qb, by = "team", all.x = TRUE)
  f <- merge(f, sk, by = "team", all.x = TRUE)
  f[is.na(qb_proj), `:=`(qb_proj = pm$qb$newcomer, qb_new = TRUE)]
  f[is.na(skill_proj), skill_proj := 0]
  # change against last season: last year's dropback leader and skill group, projected the same way
  last_qb <- proj[role == "qb" & !is.na(team1)][order(-n1)][, .SD[1], by = team1][, .(team = team1, prev_qb = proj)]
  last_sk <- proj[role %in% c("rush", "rec") & !is.na(team1), .(prev_skill = sum(proj * pmin(use_pg, 25))), by = .(team = team1)]
  f <- merge(f, last_qb, by = "team", all.x = TRUE)
  f <- merge(f, last_sk, by = "team", all.x = TRUE)
  f[, `:=`(qb_delta = qb_proj - fifelse(is.na(prev_qb), qb_proj, prev_qb),
           skill_delta = skill_proj - fifelse(is.na(prev_skill), skill_proj, prev_skill))]
  f[, c("prev_qb", "prev_skill") := NULL]
  f[, season := season]
  setnames(f, "team", "id")
  f[]
}

# Roster seen in a season's own play-by-play: everyone who touched the ball
# in the team's first n games, and the dropback leader over that stretch.
roster_from_games <- function(sd, n_games = 3) {
  pl <- sd$plays
  if (is.null(pl) || !nrow(pl)) return(NULL)
  g <- sd$games[, .(game_id, game_date)]
  pl <- merge(pl, g, by = "game_id")
  first <- unique(pl[, .(off_id, game_id, game_date)])[order(game_date)][, .SD[seq_len(min(.N, n_games))], by = off_id]
  pl1 <- pl[paste(off_id, game_id) %in% paste(first$off_id, first$game_id)]
  ro <- unique(rbind(pl1[!is.na(qb_id), .(team = off_id, id = qb_id)], pl1[!is.na(rb_id), .(team = off_id, id = rb_id)],
                     pl1[!is.na(wr_id), .(team = off_id, id = wr_id)]))
  firstq <- first[, .SD[seq_len(min(.N, PL_GAMES_FOR_STARTER))], by = off_id]
  qbs <- pl[paste(off_id, game_id) %in% paste(firstq$off_id, firstq$game_id) & !is.na(qb_id), .N, by = .(team = off_id, id = qb_id)]
  qbs <- qbs[order(-N)][, .SD[1], by = team][, .(id, team)]
  list(roster = ro, qb = qbs)
}

# --------------------------------------------------- everything, cached
build_player_history <- function(seasons_data, cache_dir) {
  path <- file.path(cache_dir, sprintf("players_%s.rds", PL_VERSION))
  have <- if (file.exists(path)) readRDS(path) else list()
  for (s in names(seasons_data)) {
    if (!is.null(have[[s]]) && as.integer(s) < CFB_SEASON) next
    message("  player model ", s)
    have[[s]] <- player_season(seasons_data[[s]], as.integer(s))
  }
  saveRDS(have[setdiff(names(have), as.character(CFB_SEASON))], path)
  list(players = rbindlist(lapply(have, `[[`, "players"), fill = TRUE),
       team_run = rbindlist(lapply(have, `[[`, "team_run"), fill = TRUE))
}

# ---------------------------------------------- roster features, all seasons
# Preseason roster for a season with no games yet. In order: a hand-kept
# file, CollegeFootballData rosters (needs cfbfastR, CFBD_API_KEY and the
# season in CFBD_SEASONS; one call), and
# last season's players on each team (assumes everyone returns; flagged).
preseason_roster <- function(season, hist, script_dir) {
  f <- file.path(script_dir, "cfb_roster_overrides.csv")
  src <- NULL; ro <- NULL
  if (exists("cfbd_ok") && cfbd_ok(season)) {
    r <- cfbd_call("rosters", season, cfbfastR::cfbd_team_roster(year = season))
    if (!is.null(r) && nrow(r)) {
      idc <- intersect(c("athlete_id", "id"), names(r))[1]; tc <- intersect(c("team_id", "teamId"), names(r))[1]
      if (!is.na(idc) && !is.na(tc)) {
        ro <- unique(r[, .(team = as.integer(get(tc)), id = suppressWarnings(as.integer(get(idc))))][!is.na(id) & !is.na(team)])
        src <- "CollegeFootballData rosters"
      }
    }
  }
  if (is.null(ro)) {
    last <- hist[season == (season - 1L)]
    ro <- unique(last[, .(team, id)])
    src <- "last season's players (no roster source; assumes everyone returns)"
  }
  qb <- data.table(team = integer(), id = integer())
  if (file.exists(f)) {
    ov <- fread(f)
    if (all(c("team_id", "athlete_id", "action") %in% names(ov))) {
      ov[, `:=`(team_id = as.integer(team_id), athlete_id = as.integer(athlete_id), action = tolower(action))]
      ro <- ro[!(paste(team, id) %in% ov[action == "exclude", paste(team_id, athlete_id)])]
      ro <- unique(rbind(ro, ov[action %in% c("add", "starting_qb"), .(team = team_id, id = athlete_id)]))
      qb <- ov[action == "starting_qb", .(team = team_id, id = athlete_id)]
      src <- paste0(src, ", with cfb_roster_overrides.csv")
    }
  }
  list(roster = ro, qb = qb, source = src)
}

# As-of roster features (c3.0). The prior for week w of a past season may only use
# what was known before week w: each team's roster and starter come from its games
# before that week (its first three, and the dropback leader in its first two, as
# in the live build). A team with no games yet gets last season's players, the same
# fallback the live build uses with no roster source. One row per season, slot and team.
roster_features_by_slot <- function(sd, hist, s, proj, pm) {
  if (is.null(sd$plays) || !nrow(sd$plays)) return(NULL)
  g <- copy(sd$games)[completed == TRUE]
  g[, slot := game_slot(g)]
  tgames <- rbind(g[, .(team = home_id, game_id, game_date, slot)], g[, .(team = away_id, game_id, game_date, slot)])
  last <- unique(hist[season == (s - 1L), .(team, id)])
  feats <- list()
  for (n in 0:3) {
    if (n == 0) {
      f <- roster_features(last, data.table(team = integer(), id = integer()), proj, pm, s)
    } else {
      rg <- roster_from_games(sd, n_games = n)
      f <- roster_features(rg$roster, rg$qb, proj, pm, s)
    }
    feats[[n + 1]] <- f[, n_asof := n]
  }
  feats <- rbindlist(feats, fill = TRUE)
  slots <- sort(unique(g$slot))
  rows <- rbindlist(lapply(slots, function(sl) {
    cnt <- tgames[slot < sl, .(n = pmin(.N, 3L)), by = team]
    teams <- unique(c(tgames$team, feats$id))
    data.table(slot = sl, id = teams, n_asof = cnt$n[match(teams, cnt$team)])[is.na(n_asof), n_asof := 0L]
  }))
  out <- merge(rows, feats, by = c("id", "n_asof"))
  # a team with games but no players seen (shouldn't happen) keeps the preseason row
  out[, season := s]
  out[]
}

all_roster_features <- function(seasons_data, hist, seasons, script_dir = ".") {
  out <- list(); meta <- list(); by_slot <- list()
  for (s in seasons) {
    if (s < min(hist$season) + 2) next
    pm <- fit_player_projection(hist[season < s])
    proj <- project_players(hist, s, pm)
    sd_s <- seasons_data[[as.character(s)]]
    rg <- if (!is.null(sd_s)) roster_from_games(sd_s) else NULL
    src <- "players in each team's first three games"
    if (is.null(rg) || !nrow(rg$roster)) { rg <- preseason_roster(s, hist, script_dir); src <- rg$source }
    out[[as.character(s)]] <- roster_features(rg$roster, rg$qb, proj, pm, s)
    if (!is.null(sd_s)) by_slot[[as.character(s)]] <- roster_features_by_slot(sd_s, hist, s, proj, pm)
    meta[[as.character(s)]] <- list(source = src, pm = pm, proj = proj, qb = rg$qb)
  }
  list(features = rbindlist(out, fill = TRUE), by_slot = rbindlist(by_slot, fill = TRUE), meta = meta)
}

# ------------------------------------------------ c3.3: expected-starting-QB features
# The margin layer gets one number per game: the QB gap, home minus away, in EPA/dropback.
#   dq = projection of the QB who plays  -  dropback-weighted projection of the QBs behind the
#        team's ratings so far (this season's earlier games; before game 1, the roster QB the
#        preseason prior assumed).
# Projections are the preseason ones (last two seasons only), so nothing here uses the future.
# Backtest: the QB who played is used as the "known by kickoff" starter, i.e. the benchmark where
# availability is known, which is what the market sees. Live: the override file, else last game's QB.
qb_proj_lookup <- function(meta_s) {
  pq <- meta_s$proj[role == "qb"]
  list(p = setNames(pq$proj, as.character(pq$id)), newc = meta_s$pm$qb$newcomer)
}
qb_p <- function(ids, lk) { v <- unname(lk$p[as.character(ids)]); v[is.na(v)] <- lk$newc; v }

qb_dropbacks <- function(sd, lk) {
  pl <- sd$plays
  if (is.null(pl) || !nrow(pl)) return(NULL)
  g <- sd$games[completed == TRUE, .(game_id, game_date)]
  q <- pl[is_pass == TRUE & !is.na(qb_id), .(N = .N), by = .(game_id, team = off_id, qb_id)]
  q <- merge(q, g, by = "game_id")
  q[, p := qb_p(qb_id, lk)]
  q[]
}

qb_game_table <- function(sd, meta_s, rs_s = NULL) {
  if (is.null(meta_s)) return(NULL)
  lk <- qb_proj_lookup(meta_s)
  q <- qb_dropbacks(sd, lk)
  if (is.null(q) || !nrow(q)) return(NULL)
  g <- copy(sd$games)[completed == TRUE]
  g[, slot := game_slot(g)]
  q <- merge(q, g[, .(game_id, slot)], by = "game_id")
  st <- q[order(-N)][, .SD[1], by = .(game_id, team)]            # the QB with the most dropbacks
  e <- q[st, on = .(team, game_date < game_date), nomatch = NULL, allow.cartesian = TRUE,
         .(game_id = i.game_id, team = i.team, N = x.N, p = x.p)]
  base <- e[, .(base = sum(N * p) / sum(N)), by = .(game_id, team)]
  out <- merge(st[, .(game_id, team, slot, qb_id, p_cur = p)], base, by = c("game_id", "team"), all.x = TRUE)
  if (!is.null(rs_s) && nrow(rs_s))
    out <- merge(out, rs_s[, .(slot, team = id, qb0 = qb_proj)], by = c("slot", "team"), all.x = TRUE)
  else out[, qb0 := NA_real_]
  out[is.na(base), base := fifelse(is.na(qb0), p_cur, qb0)]
  out[, dq := p_cur - base]
  out[, .(game_id, team, qb_id, p_cur, base, dq)]
}

# cfb_availability.csv: team_id, athlete_id, action  (action = out | starting_qb)
read_availability <- function(script_dir) {
  f <- file.path(script_dir, "cfb_availability.csv")
  if (!file.exists(f)) return(NULL)
  a <- tryCatch(fread(f), error = function(e) NULL)
  if (is.null(a) || !nrow(a) || !all(c("team_id", "athlete_id", "action") %in% names(a))) return(NULL)
  a[, action := tolower(trimws(action))]
  a[action %in% c("out", "starting_qb")]
}

qb_live_table <- function(sd_cur, meta_s, rs_s = NULL, avail = NULL) {
  lk <- qb_proj_lookup(meta_s)
  q <- qb_dropbacks(sd_cur, lk)
  if (is.null(q) || !nrow(q)) return(NULL)
  base <- q[, .(base = sum(N * p) / sum(N)), by = team]
  cur <- q[, .SD[game_date == max(game_date)], by = team][order(-N)][, .SD[1], by = team][, .(team, qb_id, p_cur = p)]
  if (!is.null(avail) && nrow(avail)) {
    for (i in seq_len(nrow(avail))) {
      a <- avail[i]; tm <- a$team_id
      if (!tm %in% cur$team) next
      if (a$action == "starting_qb") {
        cur[team == tm, `:=`(qb_id = a$athlete_id, p_cur = qb_p(a$athlete_id, lk))]
      } else if (cur[team == tm, qb_id] == a$athlete_id) {
        out_ids <- avail[team_id == tm & action == "out", athlete_id]
        alt <- q[team == tm & !qb_id %in% out_ids, .(N = sum(N), p = p[1]), by = qb_id][order(-N)]
        cur[team == tm, `:=`(qb_id = if (nrow(alt)) alt$qb_id[1] else NA_integer_,
                             p_cur = if (nrow(alt)) alt$p[1] else lk$newc)]
      }
    }
  }
  out <- merge(cur, base, by = "team")
  out[, dq := p_cur - base]
  setnames(out, "team", "id")
  out[, .(id, qb_id, p_cur, base, dq)]
}
