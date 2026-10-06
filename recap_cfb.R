# =============================================================================
# recap_cfb.R  (c3.4)
#
# Game recaps for the current season's completed games. Sourced by
# build_cfb.R, and uses its helpers (cached_download, url_pbp, num, flag,
# box_player_ids, GARBAGE_MARGIN, CACHE_DIR, CFB_SEASON, REFRESH_HOURS).
#
#   prep_recap_plays()  one pass over the season's play-by-play, cached in
#                       .cfb_cache/recap_<season>_<RECAP_VERSION>.rds: running
#                       score, line score, scoring plays, drives, per-player
#                       EPA, when garbage time began, and the game state
#                       before every play (for win probability)
#   fit_pgwe()          postgame win expectancy, fitted on past seasons
#   build_recaps()      win probability from the model's pregame line, key
#                       plays, and the compact per-game objects the page reads
#
# Nothing here changes the ratings or predictions.
# =============================================================================

RECAP_VERSION  <- "r1"
RECAP_OT_SECS  <- 240     # width of each overtime period on the win probability chart
RECAP_KEY_N    <- 5       # key plays per game
RECAP_OT_R     <- 0.02    # share of the game treated as "left" during overtime

# Rows whose score columns can't be trusted: on kickoffs the possession team is
# the receiver but the scores are the kicker's; the rest carry no game state.
RC_SCORE_SKIP <- c("Kickoff", "Kickoff Return (Offense)", "Kickoff Return Touchdown", "Timeout", "End Period",
                   "End of Half", "End of Game", "End of Regulation", "Uncategorized")
# Rows that aren't plays for the win probability chart (a penalty on a try, for
# example, carries the expected points of a snap that never happens)
RC_EV_SKIP <- c("Timeout", "Penalty", "End Period", "End of Half", "End of Game", "End of Regulation",
                "Uncategorized", "Two Point Pass", "Two Point Rush", "Defensive 2pt Conversion")

recap_text <- function(x, n = 150) {
  x <- gsub("\\s+", " ", trimws(ifelse(is.na(x), "", as.character(x))))
  # some providers' text leads with the clock and the formation and repeats the clock
  x <- sub("^\\(\\d+:\\d+\\)\\s*", "", x)
  x <- sub("^(No Huddle-?\\s*)?(Shotgun|Pistol|Under Center|No Huddle)\\s+", "", x)
  x <- gsub(",? clock \\d+:\\d+", "", x)
  # holder and snapper, tacklers in brackets, and "1ST DOWN" add nothing to a recap
  x <- gsub("\\s*\\((H|LS|Holder):[^)]*\\)", "", x)
  x <- gsub("\\s*\\(#[0-9]+[^)]*\\)", "", x)
  x <- gsub(",?\\s*1ST DOWN", "", x)
  x <- trimws(gsub("\\s+", " ", x))
  long <- nchar(x) > n
  x[long] <- paste0(sub("[ ,;]+\\S*$", "", substr(x[long], 1, n - 1)), "\u2026")
  x
}

# ------------------------------------------------------- per-season extraction
prep_recap_plays <- function(season, games) {
  games <- games[completed == TRUE & !is.na(home_pts) & !is.na(away_pts)]
  if (!nrow(games)) return(NULL)
  max_age <- if (season >= CFB_SEASON) REFRESH_HOURS else Inf
  pbp_path <- cached_download(url_pbp(season), max_age)
  if (is.null(pbp_path)) return(NULL)
  out_path <- file.path(CACHE_DIR, sprintf("recap_%d_%s.rds", season, RECAP_VERSION))
  if (file.exists(out_path) && file.mtime(out_path) >= file.mtime(pbp_path)) {
    r <- readRDS(out_path)
    if (all(games$game_id %in% r$game_ids) && identical(r$finals, games[order(game_id), paste(game_id, home_pts, away_pts)])) return(r)
  }
  message("  recaps: reading ", season, " play-by-play")
  p <- as.data.table(readRDS(pbp_path))
  p[, game_id := as.integer(game_id)]
  p <- p[game_id %in% games$game_id]
  if (!nrow(p)) return(NULL)

  # team ids and sides (pos_team is a school name)
  idmap <- rbind(games[, .(game_id, name = home_name, id = home_id, is_home = TRUE)],
                 games[, .(game_id, name = away_name, id = away_id, is_home = FALSE)])
  p <- merge(p, idmap[, .(game_id, pos_team = name, off_id = id, home_off = is_home)], by = c("game_id", "pos_team"), all.x = TRUE)
  p <- merge(p, idmap[, .(game_id, def_pos_team = name, def_id = id)], by = c("game_id", "def_pos_team"), all.x = TRUE)
  p <- merge(p, games[, .(game_id, fh = home_pts, fa = away_pts)], by = "game_id")
  # ---- play order and the clock
  # Order is the play number. A few rows in about two games in three carry a
  # period or clock from a different part of the game (a fourth-quarter snap
  # filed in the first quarter): the longest run of plays whose clock agrees
  # with the play-number order is the game's timeline, and the rest are "strays",
  # kept for drive counts but never placed on the timeline.
  p[, gpn := num(game_play_number)]
  p[, per := as.integer(period)]
  p <- p[!is.na(per)]
  setorderv(p, c("game_id", "gpn", "id_play"))
  p[, ri := seq_len(.N), by = game_id]
  p[, csec := num(clock_minutes) * 60 + num(clock_seconds)]
  p[is.na(csec) & per <= 4, csec := num(TimeSecsRem) - fifelse(per %% 2L == 1L, 900, 0)]
  p[, csec := pmin(pmax(csec, 0), 900)]
  p[, csec_f := nafill(csec, "locf"), by = .(game_id, per)]
  p[is.na(csec_f), csec_f := 900]
  p[, tkey := per * 1e4 - fifelse(per > 4, 0, csec_f)]
  lis_keep <- function(x) {
    n <- length(x); if (n < 3L) return(rep(TRUE, n))
    tails <- integer(0); tv <- numeric(0); prv <- integer(n)
    for (i in seq_len(n)) {
      pos <- findInterval(x[i], tv) + 1L
      tails[pos] <- i; tv[pos] <- x[i]
      prv[i] <- if (pos > 1L) tails[pos - 1L] else 0L
    }
    keep <- logical(n); k <- tails[length(tails)]
    while (k > 0L) { keep[k] <- TRUE; k <- prv[k] }
    keep
  }
  # timeouts, penalties and period markers carry the clock of the play before them and
  # never go on the timeline, so they don't take part in the check
  p[, stray := FALSE]
  p[!(play_type %in% RC_EV_SKIP), stray := !lis_keep(tkey), by = game_id]
  p[, grem := fifelse(per <= 4, (4L - per) * 900 + csec_f, 0)]
  p[, clock := fifelse(per > 4, "", sprintf("%d:%02d", as.integer(csec_f %/% 60), as.integer(round(csec_f %% 60))))]
  p[, t := 3600 - grem]
  # overtime has no clock to speak of: spread each period's plays evenly across its width
  p[stray == FALSE & per > 4, t := 3600 + (per[1] - 5) * RECAP_OT_SECS + RECAP_OT_SECS * seq_len(.N) / (.N + 1), by = .(game_id, per)]
  p[stray == TRUE, t := NA_real_]

  # ---- the score. The score columns are right on scoring plays (the score after
  # the play, in the possession team's terms) and wrong on a run of other rows in
  # some games, so only scoring plays are read. Points only go up, so sorting them
  # by total points puts them in the order they happened; a candidate that doesn't
  # extend the chain, or overshoots the official final, is dropped as a glitch.
  p[, scorer := flag(scoring) | grepl("Touchdown|Field Goal Good|Safety|Two Point|2pt", play_type)]
  p[, home_off := as.logical(home_off)]
  cand <- p[scorer == TRUE & !is.na(home_off) & !is.na(num(offense_score)) & !is.na(num(defense_score)),
            .(game_id, ri, per, fh, fa, type = as.character(play_type), text = recap_text(play_text, 170), drive_id = as.character(drive_id),
              kickrow = grepl("^Kickoff", play_type),
              hp = fifelse(home_off, num(offense_score), num(defense_score)), ap = fifelse(home_off, num(defense_score), num(offense_score)))]
  # on kickoff rows the scores are in the kicking team's terms, the possession team being the receiver
  cand[kickrow == TRUE, `:=`(hp = ap, ap = hp)]
  cand[, kickrow := NULL]
  cand <- cand[hp <= fh & ap <= fa & hp + ap > 0]
  chain <- function(d) {
    d <- d[order(hp + ap, ri)]; keep <- logical(nrow(d)); h <- 0; a <- 0
    for (i in seq_len(nrow(d))) if (d$hp[i] >= h && d$ap[i] >= a && (d$hp[i] > h || d$ap[i] > a)) { keep[i] <- TRUE; h <- d$hp[i]; a <- d$ap[i] }
    d[keep]
  }
  sc <- cand[, chain(.SD), by = game_id]
  # a try that sits on its own row joins the touchdown before it
  sc[, try2 := grepl("Two Point", type) & !grepl("Defensive", type)]
  sc[, grp := cumsum(!try2), by = game_id]
  sc <- sc[, .(ri = ri[1], per = per[1], hp = hp[.N], ap = ap[.N], type = type[1], text = text[1], drive_id = drive_id[1]), by = .(game_id, grp)][, grp := NULL]
  sc[, `:=`(h_pts = hp - shift(hp, fill = 0), a_pts = ap - shift(ap, fill = 0)), by = game_id]
  # time on the timeline: the row's own time, made non-decreasing through the chain
  tt <- p[, .(game_id, ri, t_row = t, clock_row = clock)]
  sc <- merge(sc, tt, by = c("game_id", "ri"), all.x = TRUE)
  sc <- sc[order(game_id, hp + ap)]
  sc[is.na(t_row), t_row := 0]
  sc[, `:=`(t = round(cummax(t_row)), per = cummax(per)), by = game_id]
  sc[, `:=`(clock = clock_row, at = ri)]
  sc[, c("t_row", "clock_row", "ri") := NULL]

  # ---- game state before each play: the score after the last scoring play before it
  p[, K := (tkey * 1e4 + ri)]
  scK <- merge(sc[, .(game_id, at, hp, ap, per)], p[, .(game_id, at = ri, K)], by = c("game_id", "at"))
  setorder(scK, game_id, K)
  q <- scK[p[, .(game_id, Kq = K - 0.5)], on = .(game_id, K = Kq), roll = TRUE]
  p[, `:=`(h0 = fifelse(is.na(q$hp), 0, q$hp), a0 = fifelse(is.na(q$ap), 0, q$ap))]
  p[, `:=`(h1 = shift(h0, -1, fill = 0), a1 = shift(a0, -1, fill = 0)), by = game_id]

  # ---- game state before each play, for win probability
  p[, kick := grepl("^Kickoff", play_type)]
  p[, epb := num(ep_before)]
  ev <- p[stray == FALSE & !(play_type %in% RC_EV_SKIP) & !is.na(play_type) & !is.na(home_off) & (kick | scorer | !is.na(epb)),
          .(game_id, ri, per, clock, t = round(t, 1), grem, h0, a0,
            E = fifelse(kick | is.na(epb), 0, epb * fifelse(home_off, 1, -1)),
            home_off, type = as.character(play_type), text = recap_text(play_text),
            cf_wp = num(home_wp_before))]

  # ---- line score, points by period; points the play-by-play never shows go
  # to the last period and are flagged
  np <- p[stray == FALSE, .(np = max(4L, max(per))), by = game_id]
  lsq <- np[, .(per = seq_len(np)), by = game_id]
  agg <- sc[, .(hq = sum(h_pts), aq = sum(a_pts)), by = .(game_id, per)]
  lsq <- merge(lsq, agg, by = c("game_id", "per"), all.x = TRUE)
  lsq[is.na(hq), hq := 0]; lsq[is.na(aq), aq := 0]
  setorder(lsq, game_id, per)
  endsc <- sc[, .(eh = max(hp), ea = max(ap)), by = game_id]
  miss <- merge(p[, .(fh = fh[1], fa = fa[1]), by = game_id], endsc, by = "game_id", all.x = TRUE)
  miss[is.na(eh), eh := 0]; miss[is.na(ea), ea := 0]
  miss <- miss[fh != eh | fa != ea][, .(game_id, miss_h = fh - eh, miss_a = fa - ea)]
  n_stray <- p[stray == TRUE, .(n = .N), by = game_id]
  message("  recaps: ", nrow(n_stray), " of ", uniqueN(p$game_id), " games have out-of-place rows (", sum(n_stray$n), " rows, left off the timeline); ",
          nrow(miss), " games end short of the official score after the scoring plays")

  # ---- drives
  p[, snapf := flag(rush) | flag(pass)]
  p[, `:=`(ytg = num(yards_to_goal), yg = num(yards_gained))]
  # where the offense got to: the end of the last snap (the drive's own end
  # column includes the punt), the goal line on a touchdown, the spot of the
  # snap on a turnover
  dr <- p[!is.na(drive_id) & !is.na(home_off), {
    hs <- home_off[snapf]; m0 <- num(drive_time_minutes_start)[1]; s0 <- num(drive_time_seconds_start)[1]
    yy <- ytg[snapf & !is.na(ytg)]; rs <- as.character(drive_result_detailed[1]); dp <- num(drive_pts)[1]
    ls_ <- tail(which(snapf & !is.na(ytg)), 1)
    e_ <- if (!length(ls_)) num(drive_start_yards_to_goal)[1]
          else if (grepl("Touchdown", rs %||% "") && isTRUE(dp > 0)) 0
          else if (grepl("Interception|Fumble", rs %||% "")) ytg[ls_]
          else min(100, max(0, ytg[ls_] - ifelse(is.na(yg[ls_]), 0, yg[ls_])))
    .(first = min(ri), cs = if (is.na(m0)) NA_real_ else m0 * 60 + ifelse(is.na(s0), 0, s0), home = if (length(hs)) hs[1] else home_off[1],
      per = { v <- as.integer(num(drive_start_period)[1]); if (is.na(v)) per[1] else v },
      clk = if (is.na(m0)) clock[1] else sprintf("%d:%02d", as.integer(m0), as.integer(ifelse(is.na(s0), 0, s0))),
      s_ytg = num(drive_start_yards_to_goal)[1], e_ytg = e_,
      deep = if (length(yy)) min(yy) else NA_real_, plays = sum(snapf), yds = num(drive_yards)[1],
      secs = num(drive_time_minutes_elapsed)[1] * 60 + num(drive_time_seconds_elapsed)[1],
      res = as.character(drive_result_detailed[1]), pts = num(drive_pts)[1])
  }, by = .(game_id, drive_id = as.character(drive_id))]
  # drives in clock order (a stray row can sit out of place in the play order)
  dr[, ord := fifelse(is.na(cs), NA_real_, per * 1e4 - cs)]
  dr[is.na(ord), ord := (per * 1e4 - 1)]
  setorder(dr, game_id, ord, first)
  dr[, c("cs", "ord") := NULL]

  # ---- per-player EPA (every scrimmage play, garbage time included, like the box score)
  s <- p[(flag(rush) | flag(pass)) & !is.na(off_id) & !is.na(def_id)]
  s[, `:=`(EPA = num(EPA), succ = flag(success), is_pass = flag(pass), is_rush = flag(rush), sk = flag(sack_vec),
           ptd = flag(pass_td), rtd = flag(rush_td), it = flag(int))]
  box_player_ids(s)
  s <- s[!is.na(EPA)]
  pe <- rbind(
    s[is_pass & !is.na(qb_i), .(role = "pass", n = .N, epa = sum(EPA), succ = sum(succ)), by = .(game_id, team = off_id, id = qb_i)],
    s[is_rush & !is.na(rb_i), .(role = "rush", n = .N, epa = sum(EPA), succ = sum(succ)), by = .(game_id, team = off_id, id = rb_i)],
    s[is_pass & !sk & !is.na(tg_i), .(role = "rec", n = .N, epa = sum(EPA), succ = sum(succ)), by = .(game_id, team = off_id, id = tg_i)])

  # ---- when garbage time began for good (Connelly's margins, as in the model)
  s[, sdiff := num(score_diff_start)]
  s[, garbage := !is.na(sdiff) & per <= 4 & abs(sdiff) > GARBAGE_MARGIN[pmin(pmax(per, 1L), 4L)]]
  setorder(s, game_id, ri)
  gt <- s[stray == FALSE, if (isTRUE(garbage[.N])) { k <- which(!garbage); j <- if (length(k)) max(k) + 1L else 1L
                                        .(t = round(t[j]), per = per[j], clock = clock[j]) }, by = game_id]

  res <- list(game_ids = games$game_id, finals = games[order(game_id), paste(game_id, home_pts, away_pts)],
              ev = ev, sc = sc, lsq = lsq[, .(game_id, per, hq, aq)], miss = miss, dr = dr, pe = pe, gt = gt)
  saveRDS(res, out_path)
  rm(p, s); gc(verbose = FALSE)
  res
}

# ------------------------------------------------- postgame win expectancy
# How often a team that played the way this one did would win: the final margin
# regressed on the gap in EPA (every scrimmage play), success rate and average
# starting field position (garbage time removed), plus home field, over every
# earlier season. Fitted on 2014-2025 it explains 84% of margins, with a
# residual spread of 9.0 points, and picks 89% of 2026's winners.
pgwe_frame <- function(x) {
  if (is.null(x$tg_all) || is.null(x$tg)) return(NULL)
  g <- x$games[completed == TRUE]
  ta <- x$tg_all[, .(game_id, off_id, epa, sr = succ / plays)]
  tc <- x$tg[, .(game_id, off_id, fp = start_ytg_sum / pmax(drives, 1), drives)]
  side <- merge(ta, tc, by = c("game_id", "off_id"), all.x = TRUE)
  side[drives == 0, fp := NA_real_]
  d <- merge(g[, .(game_id, home_id, away_id, neutral, margin = home_pts - away_pts)],
             side[, .(game_id, home_id = off_id, h_epa = epa, h_sr = sr, h_fp = fp)], by = c("game_id", "home_id"))
  d <- merge(d, side[, .(game_id, away_id = off_id, a_epa = epa, a_sr = sr, a_fp = fp)], by = c("game_id", "away_id"))
  # fp is yards to goal at the start, so the away team's minus the home team's is the home edge
  d[, .(game_id, margin, d_epa = h_epa - a_epa, d_sr = h_sr - a_sr, d_fp = a_fp - h_fp, loc = fifelse(neutral, 0, 1))]
}
fit_pgwe <- function(sd, seasons) {
  d <- rbindlist(lapply(as.character(seasons), function(s) if (!is.null(sd[[s]])) pgwe_frame(sd[[s]])))
  d <- d[is.finite(margin) & is.finite(d_epa) & is.finite(d_sr)]
  if (nrow(d) < 500) return(NULL)
  full <- lm(margin ~ d_epa + d_sr + d_fp + loc, d[is.finite(d_fp)])
  base <- lm(margin ~ d_epa + d_sr + loc, d)
  list(full = coef(full), base = coef(base), sig_full = summary(full)$sigma, sig_base = summary(base)$sigma,
       r2 = summary(full)$r.squared, n = nrow(d), seasons = range(seasons))
}
apply_pgwe <- function(m, d) {
  if (is.null(m) || is.null(d) || !nrow(d)) return(data.table(game_id = integer(), pg = numeric(), pg_m = numeric()))
  cf <- m$full; cb <- m$base
  ok <- is.finite(d$d_fp)
  pm <- ifelse(ok, cf[[1]] + cf[["d_epa"]] * d$d_epa + cf[["d_sr"]] * d$d_sr + cf[["d_fp"]] * d$d_fp + cf[["loc"]] * d$loc,
                   cb[[1]] + cb[["d_epa"]] * d$d_epa + cb[["d_sr"]] * d$d_sr + cb[["loc"]] * d$loc)
  sg <- ifelse(ok, m$sig_full, m$sig_base)
  data.table(game_id = d$game_id, pg = pnorm(pm / sg), pg_m = pm)
}

# --------------------------------------------------------- recap objects
# rp: prep_recap_plays(); g: completed games with home_pts, away_pts and the
# model's pregame home margin (pm) and phase; sigma: the layer's single-game
# spread by phase; pg: apply_pgwe(); cs: the CollegeFootballData schedule
# release (venue, attendance, notes, AP ranks), may be NULL.
#
# Win probability, home team's view, before each play:
#   P = Phi((lead + EP + pm * r) / (sigma * sqrt(r)))
# lead = home lead, EP = expected points of the possession (home positive),
# r = share of regulation left, pm and sigma = the model's pregame line and
# single-game spread. At kickoff it equals the model's pregame win chance.
# This is Stern's (1994) random-walk model with the possession added. On 2026's
# games it scores a Brier of 0.090 against 0.121 for the play-by-play's own
# win probability column, which barely moves with the line.
build_recaps <- function(rp, g, sigma, pg = NULL, cs = NULL) {
  if (is.null(rp) || !nrow(g)) return(NULL)
  r2 <- function(x, n = 2) ifelse(is.finite(x), round(x, n), NA)
  g <- copy(g)[game_id %in% rp$game_ids]
  g[, sig := vapply(phase, function(ph) { v <- sigma[[ph]]; if (is.null(v) || !is.finite(v)) sigma[["mid"]] else v }, numeric(1))]
  g[!is.finite(pm), pm := 0]
  g[, hw := home_pts > away_pts]

  ev <- merge(rp$ev, g[, .(game_id, mu = pm, sig, hw)], by = "game_id")
  setorder(ev, game_id, ri)
  ev[, r := fifelse(per > 4, RECAP_OT_R, pmax(grem / 3600, 0.001))]
  ev[, wp := pnorm((h0 - a0 + E + mu * r) / (sig * sqrt(r)))]
  ev[, wp_next := shift(wp, -1), by = game_id]
  ev[is.na(wp_next), wp_next := as.numeric(hw)]
  ev[, wpa := wp_next - wp]
  ev[, ax := abs(wpa)]
  # key plays: the biggest swings, shown in game order
  ev[, krank := frank(-ax, ties.method = "first"), by = game_id]
  ev[, key := krank <= RECAP_KEY_N & ax >= 0.03]
  gsum <- ev[, .(exc = sum(ax), lo_h = min(c(wp, wp_next)), lo_a = min(1 - c(wp, wp_next)),
                 t_end = max(t)), by = game_id]

  # thin the chart: keep a point when it has moved 0.4 points or 2.5 minutes,
  # plus scoring and key plays
  ev[, sc_row := grepl("Touchdown|Field Goal Good|Safety", type)]
  thin <- function(t, w, keep) {
    n <- length(w); out <- logical(n); last <- -1; lt <- -1e9
    for (i in seq_len(n)) if (i == 1L || i == n || keep[i] || abs(w[i] - last) >= 0.004 || t[i] - lt >= 150) {
      out[i] <- TRUE; last <- w[i]; lt <- t[i] }
    out
  }
  ev[, keep := thin(t, wp, key | sc_row | shift(sc_row, -1, fill = FALSE)), by = game_id]
  series <- ev[keep == TRUE, .(t = list(as.integer(round(t))), w = list(as.integer(round(1000 * wp)))), by = game_id]
  keyp <- ev[key == TRUE][order(game_id, ri)]

  # drive results: one dictionary, an index per drive
  dr <- rp$dr
  res_dict <- sort(unique(na.omit(dr$res)))
  dr[, res_i := match(res, res_dict) - 1L]
  # score after each drive (away, home): each scoring play belongs to the drive it happened on, and a
  # play whose drive isn't in the list takes the drive before it. Points only go up, so the score after
  # drive k is the last scoring play at or before it.
  dr[, ord_i := seq_len(.N), by = game_id]
  sdr <- merge(rp$sc[, .(game_id, drive_id, hp, ap)], dr[, .(game_id, drive_id, ord_i)], by = c("game_id", "drive_id"), all.x = TRUE)
  sdr <- sdr[order(game_id, hp + ap)]
  sdr[, ord_i := { x <- ord_i; for (i in seq_along(x)) if (is.na(x[i])) x[i] <- if (i > 1L) x[i - 1L] else 1L; cummax(x) }, by = game_id]
  sdr <- sdr[, .(hs = hp[.N], as_ = ap[.N]), by = .(game_id, ord_i)]
  dr <- merge(dr, sdr, by = c("game_id", "ord_i"), all.x = TRUE, sort = FALSE)
  setorder(dr, game_id, ord_i)
  dr[, `:=`(hs = nafill(hs, type = "locf"), as_ = nafill(as_, type = "locf")), by = game_id]
  dr[is.na(hs), hs := 0L]; dr[is.na(as_), as_ := 0L]
  sc <- merge(rp$sc, dr[, .(game_id, drive_id, d_home = home, d_plays = plays, d_yds = yds, d_secs = secs)],
              by = c("game_id", "drive_id"), all.x = TRUE)
  sc[, s_home := h_pts > a_pts]
  # a drive summary only when the scoring team was the one driving
  sc[!(d_home %in% s_home), `:=`(d_plays = NA, d_yds = NA, d_secs = NA)]
  setorder(sc, game_id, at)

  meta <- if (!is.null(cs) && nrow(cs)) {
    pick <- function(col) if (col %in% names(cs)) cs[[col]] else rep(NA, nrow(cs))
    data.table(game_id = as.integer(pick("game_id")), venue = as.character(pick("venue")),
               att = suppressWarnings(as.integer(pick("attendance"))),
               note = fcoalesce(as.character(pick("playoff_bowl_name")), as.character(pick("notes"))),
               ap_h = suppressWarnings(as.integer(pick("home_rank"))), ap_a = suppressWarnings(as.integer(pick("away_rank"))),
               cs_home = suppressWarnings(as.integer(pick("home_id"))))
  } else NULL

  ls_by <- split(rp$lsq, by = "game_id", keep.by = FALSE)
  miss_by <- if (nrow(rp$miss)) split(rp$miss, by = "game_id", keep.by = FALSE) else list()
  ser_by <- split(series, by = "game_id", keep.by = FALSE)
  sc_by <- split(sc, by = "game_id", keep.by = FALSE)
  kp_by <- split(keyp, by = "game_id", keep.by = FALSE)
  dr_by <- split(dr, by = "game_id", keep.by = FALSE)
  gt_by <- if (nrow(rp$gt)) split(rp$gt, by = "game_id", keep.by = FALSE) else list()
  gs_by <- split(gsum, by = "game_id", keep.by = FALSE)
  pg_by <- if (!is.null(pg) && nrow(pg)) split(pg, by = "game_id", keep.by = FALSE) else list()

  out <- list()
  for (i in seq_len(nrow(g))) {
    gg <- g[i]; k <- as.character(gg$game_id)
    l <- ls_by[[k]]; if (is.null(l)) next
    np <- max(4L, max(l$per))
    hq <- aq <- integer(np); hq[l$per] <- l$hq; aq[l$per] <- l$aq
    ms <- miss_by[[k]]
    if (!is.null(ms)) { hq[np] <- hq[np] + ms$miss_h; aq[np] <- aq[np] + ms$miss_a }
    o <- list(ls = list(as.integer(aq), as.integer(hq)))
    if (!is.null(ms)) o$miss <- c(as.integer(ms$miss_a), as.integer(ms$miss_h))
    s <- ser_by[[k]]
    if (!is.null(s)) {
      gs <- gs_by[[k]]
      te <- max(3600L, as.integer(ceiling(gs$t_end)))
      o$wp <- list(t = c(s$t[[1]], te), w = c(s$w[[1]], if (gg$hw) 1000L else 0L))
      o$exc <- r2(gs$exc, 2)
      o$lo <- c(r2(gs$lo_a, 3), r2(gs$lo_h, 3))
    }
    x <- sc_by[[k]]
    if (!is.null(x)) o$sc <- (x[, .(per, clock, t = as.integer(t), side = as.integer(s_home), ap = as.integer(ap), hp = as.integer(hp),
                                            text, plays = as.integer(d_plays), yds = as.integer(d_yds), secs = as.integer(d_secs))])
    x <- kp_by[[k]]
    if (!is.null(x)) o$kp <- (x[, .(per, clock, t = as.integer(round(t)), side = as.integer(home_off),
                                            w0 = as.integer(round(1000 * wp)), w1 = as.integer(round(1000 * wp_next)), text)])
    x <- dr_by[[k]]
    if (!is.null(x)) o$dr <- (x[, .(side = as.integer(home), per, clk, s = as.integer(s_ytg), e = as.integer(e_ytg),
                                            deep = as.integer(deep), plays = as.integer(plays), yds = as.integer(yds),
                                            secs = as.integer(secs), res = res_i, pts = as.integer(pts),
                                            as = as.integer(as_), hs = as.integer(hs))])
    x <- gt_by[[k]]
    if (!is.null(x)) o$gt <- list(x$t, x$per, x$clock)
    x <- pg_by[[k]]
    if (!is.null(x)) { o$pg <- r2(x$pg, 3); o$pg_m <- r2(x$pg_m, 1) }
    if (!is.null(meta)) {
      m <- meta[game_id == gg$game_id]
      if (nrow(m)) {
        flip <- !is.na(m$cs_home[1]) && m$cs_home[1] != gg$home_id
        if (!is.na(m$venue[1]) && nzchar(m$venue[1])) o$venue <- m$venue[1]
        if (!is.na(m$att[1]) && m$att[1] > 0) o$att <- m$att[1]
        if (!is.na(m$note[1]) && nzchar(m$note[1])) o$note <- m$note[1]
        ap <- if (flip) c(m$ap_h[1], m$ap_a[1]) else c(m$ap_a[1], m$ap_h[1])
        if (any(!is.na(ap))) o$ap <- ap
      }
    }
    out[[k]] <- o
  }
  list(recaps = out, dr_results = res_dict,
       wp_check = list(brier = r2(mean((ev$wp - ev$hw)^2), 4), n = nrow(ev),
                       brier_pbp = r2(ev[is.finite(cf_wp), mean((cf_wp - hw)^2)], 4)))
}
