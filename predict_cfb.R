# =============================================================================
# predict_cfb.R -- team ratings, preseason prior, walk-forward backtest and
# game predictions for FBS football. Sourced by build_cfb.R.
#
# Model in one paragraph: four opponent- and home-field-adjusted ratings
# are fitted for every team's offense and defense -- EPA per play and
# success rate (garbage time removed), points per game (every game,
# including FCS-vs-FCS) and plays per game -- by ridge regression on this
# season's games, pulled toward a preseason prior. The prior comes from the
# last two seasons, returning production, the transfer portal, the 247
# talent composite and the projected QB and skill players. A small
# regression, fitted only on earlier seasons, turns the success-rate and
# points gaps and home field into a predicted margin, with weights that
# change as the season goes on and separate weights for games against FCS
# teams; a second one gives the total, with pace. Win chance is a normal
# model on the margin, with its spread fitted out of sample. Every season in
# the backtest is predicted week by week from earlier games, earlier seasons
# and the rosters known at the time.
#
# c3.0: a review proposed ten changes. Each is a switch in CFG below so it can
# be tested alone; the ones that helped are on, the rest stay in the code
# switched off, and each switch's comment gives its backtest result (see also
# "What changed in c3.0" in the README).
# =============================================================================

PC_VERSION   <- "c3.3"   # bump to force recalibration
USE_ROSTER   <- TRUE     # roster terms (QB and skill players) in the preseason prior
K_GRID       <- c(2, 3, 4, 6, 8, 11, 15)   # prior weight, in games, tried for each rating
K_FINAL      <- 1        # prior weight for season-end ("final") ratings
BACKTEST_FROM <- 2018L   # first season scored in the Report Card
PRIOR_FROM    <- 2016L   # first season with a fitted preseason prior

# E EPA/play, S success rate, P points/game, R points/drive, N plays/game
# R (points per drive) is built and was tested in the margin, but it didn't
# help once FCS-vs-FCS games joined the points rating; add "R" here and "fR"
# to CFG$margin_terms to bring it back.
COMPONENTS <- c("E", "S", "P", "N")
MARGIN_COMPONENTS <- c(fS = "S", fP = "P", fR = "R")

# Every c3.0 change is a switch, so each can be tested alone (the harness
# turns one off and reruns the 2018-2025 backtest). Settings below are the
# ones that won; the comment gives the test. "Gap" is the model's average
# miss minus the closing line's, lined games only; SE is clustered by week.
CFG <- list(
  fcs_games    = TRUE,       # 1 KEPT. Off: gap +0.15 (SE 0.03); FCS-game gap 0.96 -> 2.07
  fcs_k_mult   = 1,          # 1 KEPT at 1. 3 (c2.1): gap +0.012 (0.007)
  fcs_prior    = TRUE,       # 1 KEPT. Off (c2.1 rule): gap +0.027 (0.014), FCS-game gap 0.96 -> 1.17
  fcs_layer    = TRUE,       # 1 KEPT. Off: gap +0.038 (0.016), weeks 1-4 FBS +0.19 (0.05)
  k_obj        = "fbs",      # 1 KEPT. "all": no difference (0.000)
  asof_roster  = TRUE,       # 2 KEPT (honesty, not accuracy). Off = the c2.1 leak: looks 0.010 better
  margin_terms = c("fS", "fP"),  # 3 DROPPED fR. With it: gap +0.009 (0.011); R instead of P: +0.23
  cfp_split    = TRUE,       # 4 KEPT. Postseason log loss 0.692 -> 0.671 (with the bowl spread in fit_pred_layer)
  team_prior_w = FALSE,      # 5 DROPPED. On: gap +0.003 (0.005)
  k_mode       = "uniform",  # c3.3: one prior weight for all components, chosen walk-forward (picks 4 almost every season).
                             # "grid" (per component, the old default): gap 0.377 vs 0.375, k drifts to the grid edge, 4x the fold-to-fold swing.
                             # "eb": gap +0.032 (0.023), mid-season worse
  w_power      = 1,          # 6 KEPT 1 (c2.1). 0.5: gap +0.010 (0.006)
  recency_hl   = Inf,        # 6 DROPPED. 8 weeks: gap -0.010 (SE 0.011), noise; bowls worse
  portal       = TRUE,       # 7 KEPT. Off: weeks 1-4 FBS gap +0.10 (0.05)
  hfa_hl       = Inf,        # 9 DROPPED (no effect: the margin layer fits its own home field)
  hfa_drop     = integer(),  # 9 DROPPED (same)
  layer_w2020  = 1,          # 9 DROPPED. 0.5: no difference
  travel       = FALSE,      # 9 DROPPED. On: gap -0.001 (0.008), noise
  gp_interact  = TRUE,       # 10 KEPT. Off: weeks 1-4 FBS gap +0.06 (0.03)
  pace_total   = TRUE,       # 10 KEPT. Off: totals miss +0.04
  fcs_nonneg   = TRUE,       # c3.3 NEW, backtest: gap to market -0.003; kept
  huber_p      = NA_real_,   # c3.3 NEW: e.g. 1.5 = Huber loss on the points ratings (changes the walk)
  fbs_level    = FALSE,       # c3.3 NEW, backtest: no gain (FCS bias flips sign by season); off
  fcs_sigma    = FALSE,       # c3.3 NEW, backtest: wider FCS sigma worsens log loss; off
  layer_decay  = NA_real_,   # c3.3 NEW: e.g. 0.8 = each season back counts 20% less in the margin-layer fit
  k_fixed      = NA_real_,   # c3.3 NEW: one fixed prior weight (games) for all components
  qb_dead      = 0,          # c3.3 NEW: QB gaps smaller than this (EPA/dropback) are treated as zero
  qb_avail     = TRUE        # c3.3 NEW, backtest: gap -0.019 pts/game (95% CI 0.002..0.037), log loss -0.001; kept
)

# Huber threshold (in MADs) for the points component only; NA = plain ridge
huber_for <- function(cmp) if (identical(cmp, "P") && is.finite(CFG$huber_p %||% NA)) CFG$huber_p else NA_real_

# True when the season has no transfer-portal rows (the file lags the season)
portal_missing <- function(side, s)
  is.null(side$portal) || !("season" %in% names(side$portal)) || !any(side$portal$season == s, na.rm = TRUE)

# ------------------------------------------------------------ observations
# Week "slot" of every game: regular-season week, postseason weeks after
# (100 + weeks since the first FBS postseason game), so later playoff rounds
# are predicted from earlier ones.
game_slot <- function(games, base = NULL) {
  post <- games$season_type == "postseason"
  if (is.null(base)) base <- if (any(post)) min(games$game_date[post], na.rm = TRUE) else NA
  wk <- if (is.na(base)) rep(0L, nrow(games)) else as.integer(floor(as.numeric(games$game_date - base) / 7))
  fifelse(post, 100L + pmax(wk, 0L), as.integer(games$week))
}

# FCS-vs-FCS games for a season, if the switch is on.
fcs_games_of <- function(sd) if (isTRUE(CFG$fcs_games) && !is.null(sd$games_fcs)) sd$games_fcs else NULL

# One row per offense per game, for one component.
make_obs <- function(sd, games, comp) {
  if (comp == "P") {
    g <- games[completed == TRUE & is.finite(home_pts) & is.finite(away_pts),
               .(game_id, home_id, away_id, neutral, home_pts, away_pts)]
    gf <- fcs_games_of(sd)
    if (!is.null(gf) && nrow(gf)) g <- rbind(g, gf[, .(game_id, home_id, away_id, neutral, home_pts, away_pts)])
    o <- rbind(g[, .(game_id, off_id = home_id, def_id = away_id, h = fifelse(neutral, 0, 1), y = home_pts)],
               g[, .(game_id, off_id = away_id, def_id = home_id, h = fifelse(neutral, 0, -1), y = away_pts)])
    o[, w := 1]
    return(o[is.finite(y)])
  }
  base <- if (comp == "N") sd$tg_all else sd$tg
  g <- games[completed == TRUE, .(game_id, home_id, away_id, neutral)]
  o <- merge(base, g, by = "game_id")
  o[, h := fifelse(neutral, 0, fifelse(off_id == home_id, 1, -1))]
  wp <- function(n, ref) ref * (n / ref)^CFG$w_power
  if (comp == "E") o[, `:=`(y = epa / plays, w = wp(plays, 62))]
  if (comp == "S") o[, `:=`(y = succ / plays, w = wp(plays, 62))]
  if (comp == "R") o[, `:=`(y = drive_pts_reg / drives_reg, w = wp(drives_reg, 12))]
  if (comp == "N") o[, `:=`(y = plays, w = 1)]
  o[is.finite(y) & w > 0, .(game_id, off_id, def_id, h, y, w)]
}

# Every game of a season with one row per game: the play-by-play games plus
# (if on) the FCS-vs-FCS games, with the columns the walk needs.
all_games <- function(sd) {
  g <- sd$games
  gf <- fcs_games_of(sd)
  if (!is.null(gf) && nrow(gf)) g <- rbind(g, gf, fill = TRUE)
  g
}

# ---------------------------------------------------------------- the fit
# Ridge regression y = mu + O[off] + D[def] + hfa*h, with each O and D
# pulled toward its prior by its own lambda (in observation-weight units).
# hfa is passed in (fitted on earlier seasons) unless hfa = NA. The league
# average mu is pulled toward mu_prior (earlier seasons' average) so that
# it is sensible in week 1, when there are no games yet. want_var = TRUE
# also returns the residual variance per unit weight and each rating's
# posterior variance (used to estimate how much the prior should weigh).
MU_K <- 60   # weight of the league-average prior, in team-games
fit_ratings <- function(obs, team_ids, prior_o, prior_d, lambda, hfa = NA, group = NULL,
                        mu_prior = 0, mu_lambda = 1e-6, lambda_d = lambda, want_var = FALSE, huber = NA) {
  nT <- length(team_ids)
  use_grp <- !is.null(group) && any(group)
  if (!is.na(hfa)) obs <- copy(obs)[, y := y - hfa * h]
  oi <- match(obs$off_id, team_ids); di <- match(obs$def_id, team_ids)
  ok <- !is.na(oi) & !is.na(di)
  oi <- oi[ok]; di <- di[ok]; yy <- obs$y[ok]; ww <- obs$w[ok]; hh <- obs$h[ok]
  n <- length(yy)
  free_hfa <- is.na(hfa)
  p0 <- 1 + 2 * nT + free_hfa
  p <- p0 + 2 * use_grp
  X <- NULL
  if (n > 0) {
    rows <- rep(seq_len(n), 3 + free_hfa)
    cols <- c(rep(1, n), 1 + oi, 1 + nT + di, if (free_hfa) rep(p0, n))
    vals <- c(rep(1, n), rep(1, n), rep(1, n), if (free_hfa) hh)
    if (use_grp) {
      go <- which(group[oi]); gd <- which(group[di])
      rows <- c(rows, go, gd); cols <- c(cols, rep(p0 + 1, length(go)), rep(p0 + 2, length(gd)))
      vals <- c(vals, rep(1, length(go) + length(gd)))
    }
    X <- Matrix::sparseMatrix(i = rows, j = cols, x = vals, dims = c(n, p))
  }
  lambda <- rep_len(lambda, nT); lambda_d <- rep_len(lambda_d, nT)
  pen <- c(mu_lambda, lambda, lambda_d, if (free_hfa) 1e-6, if (use_grp) c(1e-6, 1e-6))
  pri <- c(mu_prior, prior_o, prior_d, if (free_hfa) 0, if (use_grp) c(0, 0))
  # c3.3: the normal equations stay sparse (sparse Cholesky); the old dense as.matrix()/solve() was the
  # calibration bottleneck. Same solution to rounding.
  solve_w <- function(wv) {
    if (n > 0) {
      XtW <- Matrix::t(X * wv)
      A <- Matrix::forceSymmetric(XtW %*% X + Matrix::Diagonal(p, x = pen))
      b <- as.vector(XtW %*% yy) + pen * pri
    } else {
      A <- Matrix::Diagonal(p, x = pen); b <- pen * pri
    }
    list(A = A, beta = as.numeric(Matrix::solve(A, b)), w = wv)
  }
  sol <- solve_w(ww)
  # c3.3 (optional): Huber IRLS so one 70-0 FCS game can't drag a rating; scale = MAD of residuals
  if (is.finite(huber) && n > 0) {
    for (it in 1:3) {
      r <- yy - as.vector(X %*% sol$beta)
      cc <- huber * 1.4826 * median(abs(r))
      sol <- solve_w(ww * pmin(1, cc / pmax(abs(r), 1e-9)))
    }
  }
  A <- sol$A; beta <- sol$beta
  O <- beta[1 + seq_len(nT)]; D <- beta[1 + nT + seq_len(nT)]
  # group effects folded into each team's own rating (absolute scale)
  if (use_grp) { O <- O + beta[p0 + 1] * group; D <- D + beta[p0 + 2] * group }
  out <- list(mu = beta[1], O = O, D = D, hfa = if (free_hfa) beta[p0] else hfa, team_ids = team_ids)
  if (want_var && n > 0) {
    Ainv_d <- as.numeric(Matrix::diag(Matrix::solve(A, Matrix::Diagonal(p))))
    r <- yy - as.vector(X %*% beta)
    df <- p - sum(pen * Ainv_d)                       # effective parameters
    s2 <- sum(sol$w * r^2) / max(n - df, 1)              # residual variance per unit weight
    out$sigma2 <- s2
    out$vO <- s2 * Ainv_d[1 + seq_len(nT)]; out$vD <- s2 * Ainv_d[1 + nT + seq_len(nT)]
  }
  out
}

# lambda per team: k games' worth of the average observation weight
team_lambda <- function(k, avg_w, is_fcs) k * avg_w * ifelse(is_fcs, CFG$fcs_k_mult, 1)

# ------------------------------------------------ season-end ratings, HFA
season_teams <- function(games) {
  t <- rbind(games[, .(id = home_id, name = home_name, div = home_div, conf = home_conf, game_date)],
             games[, .(id = away_id, name = away_name, div = away_div, conf = away_conf, game_date)])
  # FBS wins: a team that played as FBS this season is FBS
  fbs_ids <- unique(t[div == "fbs", id])
  setorder(t, -game_date)
  t <- unique(t, by = "id")
  t[, game_date := NULL]
  t[, div := fifelse(id %in% fbs_ids, "fbs", "fcs")]
  t[]
}

final_ratings <- function(sd) {
  games <- sd$games[completed == TRUE]
  teams <- season_teams(all_games(list(games = games, games_fcs = sd$games_fcs)))
  out <- list()
  for (cmp in COMPONENTS) {
    obs <- make_obs(sd, games, cmp)
    aw <- mean(obs$w)
    f <- fit_ratings(obs, teams$id, rep(0, nrow(teams)), rep(0, nrow(teams)),
                     K_FINAL * aw, hfa = NA, group = teams$div == "fcs", want_var = TRUE, huber = huber_for(cmp))
    out[[cmp]] <- data.table(id = teams$id, O = f$O, D = f$D, vO = f$vO, vD = f$vD)
    out[[paste0(cmp, "_meta")]] <- list(mu = f$mu, hfa = f$hfa, avg_w = aw, sigma2 = f$sigma2)
  }
  out$teams <- teams
  out
}

# Home field for season s: a weighted average of earlier seasons' fitted
# values, recent seasons counting more, with CFG$hfa_drop left out.
hfa_for <- function(hfa_hist, s) {
  yrs <- as.integer(names(hfa_hist))
  keep <- yrs < s & !(yrs %in% CFG$hfa_drop)
  if (!any(keep)) keep <- yrs < s
  w <- 0.5^((s - 1 - yrs[keep]) / CFG$hfa_hl)
  colSums(do.call(rbind, hfa_hist[keep]) * w) / sum(w)
}

# ------------------------------------------------------- preseason prior
# Builds one row per team in season s with the inputs to the prior. With
# slot given (and as-of rosters on), the roster terms are the ones known
# before that week.
prior_inputs <- function(s, finals, side, teams_s, slot = NULL) {
  get_prev <- function(yr, cmp) {
    f <- finals[[as.character(yr)]]
    if (is.null(f)) return(data.table(id = integer(), O = numeric(), D = numeric()))
    f[[cmp]]
  }
  rows <- data.table(id = teams_s$id, div = teams_s$div)
  for (cmp in COMPONENTS) {
    p1 <- get_prev(s - 1, cmp); p2 <- get_prev(s - 2, cmp)
    rows <- merge(rows, p1[, .(id, o1 = O, d1 = D)], by = "id", all.x = TRUE)
    rows <- merge(rows, p2[, .(id, o2 = O, d2 = D)], by = "id", all.x = TRUE)
    setnames(rows, c("o1", "d1", "o2", "d2"), paste0(cmp, c("_o1", "_d1", "_o2", "_d2")))
  }
  s_ <- s
  tal <- side$talent[season == s_, .(id = team_id, talent)]
  ret <- side$returning[season == s_, .(id = team_id, ret_off, ret_def)]
  rows <- merge(rows, tal, by = "id", all.x = TRUE)
  rows <- merge(rows, ret, by = "id", all.x = TRUE)
  if (!is.null(side$portal) && nrow(side$portal)) {
    po <- side$portal[season == s_, .(id = team_id, portal_share, tal_in, tal_out)]
    rows <- merge(rows, unique(po, by = "id"), by = "id", all.x = TRUE)
  } else rows[, `:=`(portal_share = NA_real_, tal_in = NA_real_, tal_out = NA_real_)]
  if (USE_ROSTER && !is.null(side$roster)) {
    src <- side$roster[season == s_]
    if (!is.null(slot) && isTRUE(CFG$asof_roster) && !is.null(side$roster_slot) && nrow(side$roster_slot)) {
      sl_ <- slot; s_ <- s
      rs <- side$roster_slot[season == s_ & slot == sl_]
      if (nrow(rs)) src <- rs
    }
    rf <- src[, .(id, qb_proj, qb_new = as.numeric(qb_new), skill_proj, qb_delta, skill_delta)]
    rows <- merge(rows, unique(rf, by = "id"), by = "id", all.x = TRUE)
  }
  rows[, season := s]
  rows
}

# Fills gaps so every FBS team has usable inputs: a team with no FBS
# history (new to FBS) gets the 15th percentile of FBS teams; missing
# talent, returning production or portal data gets the FBS median.
fill_inputs <- function(r) {
  r <- copy(r)
  fbs <- r$div == "fbs"
  for (cmp in COMPONENTS) for (sfx in c("_o1", "_d1", "_o2", "_d2")) {
    col <- paste0(cmp, sfx)
    q <- quantile(r[[col]][fbs], if (grepl("_o", sfx)) 0.15 else 0.85, na.rm = TRUE)
    qf <- median(r[[col]][!fbs], na.rm = TRUE)
    if (!is.finite(q)) q <- 0
    if (!is.finite(qf)) qf <- q
    r[is.na(get(col)) & div == "fbs", (col) := q]
    r[is.na(get(col)), (col) := qf]
  }
  zs <- function(x) { m <- mean(x[fbs], na.rm = TRUE); s <- sd(x[fbs], na.rm = TRUE)
    if (!is.finite(s) || s == 0) return(rep(NA_real_, length(x))); (x - m) / s }
  r[, talent_z := zs(talent)]
  r[is.na(talent_z), talent_z := fifelse(div == "fbs", -0.5, -2)]
  med_o <- median(r$ret_off[fbs], na.rm = TRUE); med_d <- median(r$ret_def[fbs], na.rm = TRUE)
  if (!is.finite(med_o)) med_o <- 0.6
  if (!is.finite(med_d)) med_d <- 0.6
  r[is.na(ret_off), ret_off := med_o]; r[is.na(ret_def), ret_def := med_d]
  # transfer portal: incoming and outgoing 247 points (z-scored among FBS teams)
  # and the share of the roster that came through the portal; 0 when missing
  r[, `:=`(tin_z = zs(tal_in), tout_z = zs(tal_out), psh = portal_share - median(portal_share[fbs], na.rm = TRUE))]
  for (cc in c("tin_z", "tout_z", "psh")) r[!is.finite(get(cc)), (cc) := 0]
  if (USE_ROSTER) {
    if (!"qb_proj" %in% names(r)) r[, `:=`(qb_proj = NA_real_, qb_new = NA_real_, skill_proj = NA_real_, qb_delta = NA_real_, skill_delta = NA_real_)]
    q25 <- quantile(r$qb_proj[fbs], 0.25, na.rm = TRUE); if (!is.finite(q25)) q25 <- 0
    r[is.na(qb_proj), `:=`(qb_proj = q25, qb_new = 1)]
    r[is.na(skill_proj), skill_proj := 0]
    r[is.na(qb_delta), qb_delta := 0]; r[is.na(skill_delta), skill_delta := 0]
  }
  r[]
}

prior_design <- function(r, cmp, side) {
  pt <- isTRUE(CFG$portal)
  if (side == "o") {
    d <- data.table(x1 = r[[paste0(cmp, "_o1")]], x2 = r[[paste0(cmp, "_o2")]], tz = r$talent_z,
                    ret = r$ret_off - 0.6, x1ret = r[[paste0(cmp, "_o1")]] * (r$ret_off - 0.6))
    # roster terms: projected starting QB (EPA/dropback above average), whether he
    # has no FBS history, and projected skill-player value per game
    if (USE_ROSTER) d[, `:=`(qb = r$qb_proj, qbnew = r$qb_new, skill = r$skill_proj, qbd = r$qb_delta, skd = r$skill_delta)]
  } else {
    d <- data.table(x1 = r[[paste0(cmp, "_d1")]], x2 = r[[paste0(cmp, "_d2")]], tz = r$talent_z,
                    ret = r$ret_def - 0.6, x1ret = r[[paste0(cmp, "_d1")]] * (r$ret_def - 0.6))
  }
  if (pt) d[, `:=`(tin = r$tin_z, tout = r$tout_z, psh = r$psh)]
  d
}

# Inputs to the model of how far off a team's prior is likely to be: a team
# with little returning production, a new QB or heavy portal turnover gets
# a weaker prior (component 5).
prior_var_design <- function(r, side) {
  d <- data.table(lowret = pmax(0.6 - (if (side == "o") r$ret_off else r$ret_def), 0),
                  psh = abs(r$psh))
  if (side == "o" && USE_ROSTER) d[, `:=`(qbnew = r$qb_new, qbd = abs(r$qb_delta))]
  d
}

# Fits the prior regressions on every FBS team-season in train_seasons
# (and, for FCS teams, a smaller regression on FCS team-seasons). Also fits
# the prior-error variance model and the empirical-Bayes prior strength.
fit_prior_model <- function(train_seasons, finals, side, teams_by_season) {
  models <- list()
  for (cmp in COMPONENTS) for (sd_ in c("o", "d")) {
    X <- list(); Y <- c(); V <- c(); XF <- list(); YF <- c(); VR <- list()
    for (s in train_seasons) {
      f <- finals[[as.character(s)]]
      if (is.null(f) || is.null(finals[[as.character(s - 1)]])) next
      r <- fill_inputs(prior_inputs(s, finals, side, teams_by_season[[as.character(s)]]))
      i <- match(r$id, f[[cmp]]$id)
      tgt <- if (sd_ == "o") f[[cmp]]$O[i] else f[[cmp]]$D[i]
      pv <- if (sd_ == "o") f[[cmp]]$vO[i] else f[[cmp]]$vD[i]
      ok <- !is.na(tgt) & r$div == "fbs"
      X[[length(X) + 1]] <- prior_design(r, cmp, sd_)[ok]
      VR[[length(VR) + 1]] <- prior_var_design(r, sd_)[ok]
      Y <- c(Y, tgt[ok]); V <- c(V, pv[ok])
      okf <- !is.na(tgt) & r$div == "fcs" & !is.na(finals[[as.character(s - 1)]][[cmp]]$O[match(r$id, finals[[as.character(s - 1)]][[cmp]]$id)])
      if (any(okf)) {
        XF[[length(XF) + 1]] <- prior_design(r, cmp, sd_)[okf, .(x1, x2)]
        YF <- c(YF, tgt[okf])
      }
    }
    if (!length(X)) { models[[paste0(cmp, sd_)]] <- NULL; next }
    X <- rbindlist(X)
    fit <- lm(Y ~ ., data = cbind(X, Y = Y))
    res <- resid(fit)
    m <- list(coef = coef(fit), resid_sd = sd(res), n = length(Y))
    # empirical Bayes: the spread of true prior misses is the spread of
    # residuals less the noise in the season-end ratings themselves
    m$tau2 <- max(var(res) - mean(V, na.rm = TRUE), 0.1 * var(res))
    # prior-error variance model (gamma GLM, log link, on squared residuals)
    if (isTRUE(CFG$team_prior_w)) {
      VR <- rbindlist(VR)
      vfit <- tryCatch(glm(r2 ~ ., data = cbind(VR, r2 = pmax(res^2, 1e-12)), family = Gamma(link = "log"),
                           control = glm.control(maxit = 50)), error = function(e) NULL)
      if (!is.null(vfit)) m$vcoef <- coef(vfit)
    }
    if (isTRUE(CFG$fcs_prior) && length(YF) >= 100) {
      XF <- rbindlist(XF)
      m$fcs_coef <- coef(lm(YF ~ ., data = cbind(XF, YF = YF)))
    }
    models[[paste0(cmp, sd_)]] <- m
  }
  models
}

# Prior for every team (FBS and FCS) in season s. Columns <cmp>_o and <cmp>_d
# are the prior ratings; w_<cmp>_o and w_<cmp>_d multiply each team's prior
# weight (1 = average uncertainty).
make_prior <- function(s, finals, side, teams_s, pmodel, slot = NULL) {
  r <- fill_inputs(prior_inputs(s, finals, side, teams_s, slot))
  out <- data.table(id = r$id, div = r$div)
  for (cmp in COMPONENTS) for (sd_ in c("o", "d")) {
    m <- pmodel[[paste0(cmp, sd_)]]
    X <- prior_design(r, cmp, sd_)
    if (is.null(m)) {
      val <- 0.6 * X$x1   # no trained model yet (first season): plain regression to the mean
    } else {
      cf <- m$coef; cf[is.na(cf)] <- 0
      val <- rep(cf[1], nrow(X))
      for (nm in setdiff(names(cf), "(Intercept)")) if (nm %in% names(X)) val <- val + cf[nm] * X[[nm]]
    }
    fcs <- r$div == "fcs"
    if (!is.null(m$fcs_coef)) {
      cf <- m$fcs_coef; cf[is.na(cf)] <- 0
      vf <- cf[1] + cf["x1"] * X$x1 + cf["x2"] * X$x2
      val <- ifelse(fcs, vf, val)
    } else {
      # FCS teams: the FCS average plus half of last season's gap from it
      fm <- mean(X$x1[fcs])
      if (!is.finite(fm)) fm <- 0
      val <- ifelse(fcs, fm + 0.5 * (X$x1 - fm), val)
    }
    out[, (paste0(cmp, "_", sd_)) := as.numeric(val)]
    mult <- rep(1, nrow(r))
    if (isTRUE(CFG$team_prior_w) && !is.null(m$vcoef)) {
      VX <- prior_var_design(r, sd_)
      cf <- m$vcoef; cf[is.na(cf)] <- 0
      lv <- rep(cf[1], nrow(VX))
      for (nm in setdiff(names(cf), "(Intercept)")) if (nm %in% names(VX)) lv <- lv + cf[nm] * VX[[nm]]
      v <- exp(lv)
      mult <- pmin(pmax(mean(v[!fcs]) / v, 0.5), 2)
      mult[fcs] <- 1
    }
    out[, (paste0("w_", cmp, "_", sd_)) := mult]
  }
  out
}

# Empirical-Bayes prior strength, in games, for each component: residual
# variance per unit weight over the variance of true prior misses.
eb_k <- function(finals, pmodel, seasons) {
  sapply(setNames(COMPONENTS, COMPONENTS), function(cmp) {
    s2 <- mean(sapply(as.character(seasons), function(s) finals[[s]][[paste0(cmp, "_meta")]]$sigma2), na.rm = TRUE)
    aw <- mean(sapply(as.character(seasons), function(s) finals[[s]][[paste0(cmp, "_meta")]]$avg_w), na.rm = TRUE)
    tau2 <- mean(c(pmodel[[paste0(cmp, "o")]]$tau2, pmodel[[paste0(cmp, "d")]]$tau2))
    s2 / (tau2 * aw)
  })
}

# ------------------------------------------------- weekly in-season fits
# Travel: great-circle miles from each team's home stadium to the venue.
haversine_mi <- function(lat1, lon1, lat2, lon2) {
  rad <- pi / 180
  a <- sin((lat2 - lat1) * rad / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin((lon2 - lon1) * rad / 2)^2
  3958.8 * 2 * asin(pmin(1, sqrt(a)))
}
# log(1 + away team's miles) minus log(1 + home team's miles); 0 when unknown
travel_term <- function(g, coords) {
  if (is.null(coords) || !nrow(coords) || !nrow(g)) return(rep(0, nrow(g)))
  hi <- match(g$home_id, coords$team_id); ai <- match(g$away_id, coords$team_id)
  vid <- if ("venue_id" %in% names(g)) g$venue_id else rep(NA_integer_, nrow(g))
  vi <- match(vid, coords$venue_id)
  vlat <- ifelse(g$neutral, coords$lat[vi], coords$lat[hi]); vlon <- ifelse(g$neutral, coords$lon[vi], coords$lon[hi])
  dh <- haversine_mi(coords$lat[hi], coords$lon[hi], vlat, vlon)
  da <- haversine_mi(coords$lat[ai], coords$lon[ai], vlat, vlon)
  out <- log1p(da) - log1p(dh)
  out[!is.finite(out)] <- 0
  out
}

# For season s, for each slot with games, fits ratings on completed games
# from earlier slots. Returns pregame features for every game (all k's).
# prior_at(slot) gives the prior in force for that week.
season_walk <- function(sd, prior_at, hfa_by_cmp, teams_s, k_grid = K_GRID, slots_to_score = NULL, mu_by_cmp) {
  games <- copy(sd$games)
  post <- games$season_type == "postseason"
  base <- if (any(post)) min(games$game_date[post], na.rm = TRUE) else NULL
  games[, slot := game_slot(games, base)]
  ga <- all_games(list(games = games[completed == TRUE], games_fcs = sd$games_fcs))
  ga[, slot := game_slot(ga, base)]
  slots <- sort(unique(games$slot))
  if (!is.null(slots_to_score)) slots <- intersect(slots, slots_to_score)
  team_ids <- teams_s$id
  is_fcs <- teams_s$div == "fcs"
  obs_all <- lapply(setNames(COMPONENTS, COMPONENTS), function(cmp) {
    o <- make_obs(sd, games[completed == TRUE], cmp)
    merge(o, ga[, .(game_id, slot, game_date)], by = "game_id")
  })
  avg_w <- sapply(obs_all, function(o) if (nrow(o)) mean(o$w) else NA_real_)
  dflt <- c(E = 62, S = 62, P = 1, R = 12, N = 1)
  avg_w[is.na(avg_w)] <- dflt[names(avg_w)[is.na(avg_w)]]
  # games played before each slot, per team (all games, FCS-vs-FCS included)
  tgp <- rbind(ga[, .(id = home_id, slot, game_date)], ga[, .(id = away_id, slot, game_date)])

  feats <- list()
  for (sl in slots) {
    gs <- games[slot == sl]
    hi <- match(gs$home_id, team_ids); ai <- match(gs$away_id, team_ids)
    pr <- prior_at(sl)
    pr <- pr[match(team_ids, pr$id)]
    cut_date <- min(gs$game_date, na.rm = TRUE)
    # as-of cut by date, not week label: a postponed game labelled earlier can't leak in
    gpc <- tgp[(game_date < cut_date) %in% TRUE | (is.na(game_date) & slot < sl), .N, by = id]
    gp <- gpc$N[match(team_ids, gpc$id)]; gp[is.na(gp)] <- 0
    obs_w <- lapply(obs_all, function(o) {
      o <- o[(game_date < cut_date) %in% TRUE | (is.na(game_date) & slot < sl)]
      if (is.finite(CFG$recency_hl) && nrow(o)) o <- copy(o)[, w := w * 0.5^(as.numeric(cut_date - game_date) / 7 / CFG$recency_hl)]
      o
    })
    for (k in k_grid) {
      row <- data.table(game_id = gs$game_id, k = k, gp_h = gp[hi], gp_a = gp[ai])
      for (cmp in COMPONENTS) {
        lam <- team_lambda(k, avg_w[[cmp]], is_fcs)
        f <- fit_ratings(obs_w[[cmp]], team_ids, pr[[paste0(cmp, "_o")]], pr[[paste0(cmp, "_d")]],
                         lam * pr[[paste0("w_", cmp, "_o")]], hfa = hfa_by_cmp[[cmp]],
                         mu_prior = mu_by_cmp[[cmp]], mu_lambda = MU_K * avg_w[[cmp]],
                         lambda_d = lam * pr[[paste0("w_", cmp, "_d")]], huber = huber_for(cmp))
        net <- f$O - f$D
        row[, (paste0("f", cmp)) := net[hi] - net[ai]]
        row[, (paste0("t", cmp)) := 2 * f$mu + f$O[hi] + f$D[ai] + f$O[ai] + f$D[hi]]
      }
      feats[[length(feats) + 1]] <- row
    }
  }
  list(features = rbindlist(feats), games = games)
}

# ------------------------------------------------------ prediction layer
# margin = b_loc*loc + b_trav*trav + sum over rating gaps of (b + g*x)*gap,
# with separate b's for games involving an FCS team. x is how far into the
# season the two teams are (average games played, capped at 10, over 10).
# No intercept: the margin is symmetric.
# total  = a0 + aP*tP + aE*tE (+ aN*tN + aEN*tE*tN/100 with pace)
margin_design <- function(d, terms = CFG$margin_terms) {
  fbs <- if ("fbs_game" %in% names(d)) d$fbs_game %in% TRUE else rep(TRUE, nrow(d))
  x <- pmin((d$gp_h + d$gp_a) / 2, 10) / 10
  M <- list(loc = d$loc)
  if (isTRUE(CFG$travel)) M$trav <- if ("trav" %in% names(d)) d$trav else rep(0, nrow(d))
  for (t in terms) {
    v <- d[[t]]
    if (isTRUE(CFG$fcs_layer)) {
      M[[t]] <- v * fbs; M[[paste0(t, "_fcs")]] <- v * !fbs
      if (isTRUE(CFG$gp_interact)) M[[paste0(t, "_x")]] <- v * x * fbs
    } else {
      M[[t]] <- v
      if (isTRUE(CFG$gp_interact)) M[[paste0(t, "_x")]] <- v * x
    }
  }
  # c3.3: signed FBS-vs-FCS level (+1 only the home team is FBS, -1 only the away team) and the
  # expected-starting-QB gap (home minus away, EPA/dropback; FBS games only)
  if (isTRUE(CFG$fbs_level)) { v <- if ("lvl" %in% names(d)) d$lvl else rep(0, nrow(d)); v[is.na(v)] <- 0; M$lvl <- v }
  if (isTRUE(CFG$qb_avail)) { v <- if ("qbd" %in% names(d)) d$qbd else rep(0, nrow(d)); v[is.na(v)] <- 0; dz <- CFG$qb_dead %||% 0; if (dz > 0) v <- sign(v) * pmax(abs(v) - dz, 0)   # ignore projection noise below dz
    M$qbd <- v * fbs }
  as.matrix(as.data.frame(M))
}
total_design <- function(d) {
  M <- list(int = rep(1, nrow(d)), tP = d$tP, tE = d$tE)
  if (isTRUE(CFG$pace_total)) { M$tN <- d$tN; M$tEN <- d$tE * d$tN / 100 }
  as.matrix(as.data.frame(M))
}
# c3.3: optional recency decay (per season) so the layer can follow drift such as the FBS-vs-FCS gap
layer_weights <- function(d) ifelse(d$season == 2020, CFG$layer_w2020, 1) *
  (if (is.finite(CFG$layer_decay %||% NA)) CFG$layer_decay^(max(d$season) - d$season) else 1)
wls <- function(M, y, w) { f <- lm.wfit(M, y, w); cf <- f$coefficients; cf[is.na(cf)] <- 0; cf }

# Win-chance spread: maximum likelihood on wins and losses. The playoff
# (4: no opt-outs) uses the in-season spread; other bowls get their own,
# fitted when there are enough of them.
fit_sigma <- function(pm, margin) {
  nll <- function(s) -sum(pnorm(pm / s, log.p = TRUE)[margin > 0]) - sum(pnorm(-pm / s, log.p = TRUE)[margin < 0])
  optimize(nll, c(5, 40))$minimum
}
fit_pred_layer <- function(d) {
  w <- layer_weights(d)
  M <- margin_design(d); cm <- wls(M, d$margin, w)
  if (isTRUE(CFG$fcs_nonneg)) {
    neg <- (grepl("_fcs$", colnames(M)) | colnames(M) == "qbd") & cm[colnames(M)] < 0
    if (any(neg)) { cm2 <- wls(M[, !neg, drop = FALSE], d$margin, w); cm[] <- 0; cm[names(cm2)] <- cm2 }
  }
  Tm <- total_design(d); ct <- wls(Tm, d$total, w)
  d <- copy(d)
  d[, pm := as.vector(M %*% cm)]
  ph <- d$phase
  sig <- c(early = NA, mid = NA, cfp = NA, post = NA, fcs = NA)
  fb <- d$fbs_game %in% TRUE; sg <- isTRUE(CFG$fcs_sigma)
  sel <- function(p_) if (sg) ph == p_ & fb else ph == p_
  for (p_ in c("early", "mid")) sig[p_] <- fit_sigma(d$pm[sel(p_)], d$margin[sel(p_)])
  # c3.3: games with an FCS team miss by more (14.2 vs 12.1 in 2026); their own spread
  fc <- !fb & ph %in% c("early", "mid")
  if (sg && sum(fc) >= 40) sig["fcs"] <- fit_sigma(d$pm[fc], d$margin[fc])
  sig["cfp"] <- sig["mid"]
  bowls <- ph == "post"
  if (isTRUE(CFG$cfp_split)) {
    # Bowls other than the playoff: too few (about 35 a season, from 2023) to fit
    # the spread on wins and losses alone -- on 2023-2025 that estimate runs off
    # to 60+ points. Instead scale the in-season spread by how much bigger the
    # misses on the margin are in bowls (opt-outs, coaching changes).
    mid_ <- sel("mid")
    sig["post"] <- if (sum(bowls) >= 30)
      sig["mid"] * sd(d$margin[bowls] - d$pm[bowls]) / sd(d$margin[mid_] - d$pm[mid_]) else 1.2 * sig["mid"]
  } else {
    pp <- ph %in% c("post", "cfp")
    sig["post"] <- if (sum(pp) >= 50) fit_sigma(d$pm[pp], d$margin[pp]) else fit_sigma(d$pm, d$margin)
    sig["post"] <- min(sig["post"], 1.35 * sig["mid"])
    sig["cfp"] <- sig["post"]
  }
  list(margin = cm, total = ct, sigma = sig,
       resid_margin = sd(d$margin - d$pm), resid_total = sd(d$total - as.vector(Tm %*% ct)), n_bowls = sum(bowls))
}

apply_pred_layer <- function(d, L) {
  M <- margin_design(d, sub("_fcs$|_x$", "", grep("^f", names(L$margin), value = TRUE)) |> unique())
  M <- M[, names(L$margin), drop = FALSE]
  Tm <- total_design(d)[, names(L$total), drop = FALSE]
  d[, pm := as.vector(M %*% L$margin)]
  d[, pt := as.vector(Tm %*% L$total)]
  d[, sig := L$sigma[phase]]
  if (isTRUE(CFG$fcs_sigma) && "fbs_game" %in% names(d) && is.finite(L$sigma["fcs"]))
    d[!(fbs_game %in% TRUE) & phase %in% c("early", "mid"), sig := L$sigma[["fcs"]]]
  d[, p_home := pnorm(pm / sig)]
  d
}

phase_of <- function(week, season_type, cfp = FALSE) {
  fifelse(season_type == "postseason", fifelse(cfp %in% TRUE, "cfp", "post"), fifelse(week <= 4, "early", "mid"))
}

# Points per game for each team's offense (off) and defense (def) on the
# margin's scale, for a game between FBS teams at season progress x.
net_scale <- function(comp_fits, L, x = 1) {
  cm <- L$margin
  off <- 0; def <- 0
  for (t in names(MARGIN_COMPONENTS)) {
    if (!t %in% names(cm)) next
    b <- cm[[t]] + (if (paste0(t, "_x") %in% names(cm)) cm[[paste0(t, "_x")]] * x else 0)
    f <- comp_fits[[MARGIN_COMPONENTS[[t]]]]
    off <- off + b * f$O; def <- def + b * f$D
  }
  list(off = off, def = def)
}

# --------------------------------------------------------- full pipeline
# seasons_data: named list of prep_season() outputs. Returns everything
# the dashboard needs. Calibration (the expensive walk) is cached.
run_cfb_model <- function(seasons_data, side, current_season, cache_dir) {
  seasons <- sort(as.integer(names(seasons_data)))
  past <- seasons[seasons < current_season]
  cal_path <- file.path(cache_dir, sprintf("calibration_%d_%s.rds", current_season, PC_VERSION))

  if (file.exists(cal_path)) {
    message("  using cached calibration (", basename(cal_path), ")")
    cal <- readRDS(cal_path)
  } else {
    message("  calibrating on ", min(past), "-", max(past), " (first run of the season; a few minutes)")
    finals <- list(); teams_by <- list(); hfa_hist <- list(); mu_hist <- list()
    for (s in past) {
      finals[[as.character(s)]] <- f <- final_ratings(seasons_data[[as.character(s)]])
      teams_by[[as.character(s)]] <- f$teams
      hfa_hist[[as.character(s)]] <- sapply(COMPONENTS, function(c) f[[paste0(c, "_meta")]]$hfa)
      mu_hist[[as.character(s)]] <- sapply(COMPONENTS, function(c) f[[paste0(c, "_meta")]]$mu)
    }
    feats <- list(); priors <- list(); pmodels <- list(); ebk <- list()
    for (s in past[past >= PRIOR_FROM]) {
      # prior regressions need two earlier seasons of ratings for every row
      tr <- past[past < s & past >= FIRST_SEASON + 2]
      pm <- fit_prior_model(tr, finals, side, teams_by)
      pmodels[[as.character(s)]] <- pm
      ebk[[as.character(s)]] <- if (length(tr)) eb_k(finals, pm, tr) else NULL
      sd_s <- seasons_data[[as.character(s)]]
      teams_s <- season_teams(all_games(sd_s))
      prior <- make_prior(s, finals, side, teams_s, pm)
      priors[[as.character(s)]] <- prior
      prior_cache <- list()
      prior_at <- function(sl) {
        if (!isTRUE(CFG$asof_roster)) return(prior)
        key <- as.character(sl)
        if (is.null(prior_cache[[key]])) prior_cache[[key]] <<- make_prior(s, finals, side, teams_s, pm, slot = sl)
        prior_cache[[key]]
      }
      hfa_prev <- hfa_for(hfa_hist, s)
      # league average: the last two seasons (scoring drifts over time)
      mu_prev <- colMeans(do.call(rbind, mu_hist[as.character(intersect(past[past < s], c(s - 1, s - 2)))]))
      w <- season_walk(sd_s, prior_at, as.list(hfa_prev), teams_s, mu_by_cmp = as.list(mu_prev))
      g <- w$games[completed == TRUE]
      g[, trav := travel_term(g, side$coords)]
      ff <- merge(w$features, g[, .(game_id, season, week, season_type, neutral, spread, over_under, cfp, trav,
                                    home_id, away_id, margin = home_pts - away_pts, total = home_pts + away_pts,
                                    fbs_game = home_div == "fbs" & away_div == "fbs")], by = "game_id")
      feats[[as.character(s)]] <- ff
      message("    ", s, ": ", uniqueN(ff$game_id), " games walked")
    }
    cal <- list(finals = finals, teams_by = teams_by, hfa_hist = hfa_hist, mu_hist = mu_hist, feats = rbindlist(feats),
                priors = priors, pmodels = pmodels, ebk = ebk, cfg = CFG)
    saveRDS(cal, cal_path)
  }
  fit_layers(cal, layer_extras(seasons_data, side, unique(cal$feats$season)))
}

# c3.3: per-game margin-layer inputs that don't depend on the (cached) walk: the FBS-vs-FCS level
# and the expected-starting-QB gap. Cheap, so the calibration cache stays valid across changes here.
layer_extras <- function(seasons_data, side, seasons) {
  out <- list()
  for (s in seasons) {
    sd_s <- seasons_data[[as.character(s)]]
    if (is.null(sd_s)) next
    ag <- all_games(sd_s)[, .(game_id, home_div, away_div)]
    o <- ag[, .(game_id, lvl = fifelse(home_div == "fbs" & away_div != "fbs", 1,
                                       fifelse(home_div != "fbs" & away_div == "fbs", -1, 0)))]
    if (exists("qb_game_table") && !is.null(side$meta[[as.character(s)]])) {     # computed whatever the switch says; the switch gates the margin term
      rs <- if (!is.null(side$roster_slot)) side$roster_slot[season == s] else NULL
      qg <- tryCatch(qb_game_table(sd_s, side$meta[[as.character(s)]], rs), error = function(e) {
        message("  qb features skipped for ", s, ": ", conditionMessage(e)); NULL })
      if (!is.null(qg) && nrow(qg)) {
        gm <- sd_s$games[, .(game_id, home_id, away_id)]
        dh <- qg[, .(game_id, home_id = team, dq_h = dq)]; da <- qg[, .(game_id, away_id = team, dq_a = dq)]
        gm <- merge(gm, dh, by = c("game_id", "home_id"), all.x = TRUE)
        gm <- merge(gm, da, by = c("game_id", "away_id"), all.x = TRUE)
        gm[, qbd := fifelse(is.na(dq_h), 0, dq_h) - fifelse(is.na(dq_a), 0, dq_a)]
        o <- merge(o, gm[, .(game_id, qbd)], by = "game_id", all.x = TRUE)
      }
    }
    out[[as.character(s)]] <- o
  }
  rbindlist(out, fill = TRUE)
}

# Chooses k per component and fits the prediction layer, walk-forward.
# Split from run_cfb_model so layer-only settings can be tested on one walk.
fit_layers <- function(cal, extra = NULL) {
  feats <- copy(cal$feats)
  if (!is.null(extra) && nrow(extra)) feats <- merge(feats, unique(extra, by = "game_id"), by = "game_id", all.x = TRUE)
  for (cn in c("lvl", "qbd")) if (!cn %in% names(feats)) feats[, (cn) := 0] else feats[is.na(get(cn)), (cn) := 0]
  feats[, `:=`(loc = fifelse(neutral, 0, 1), phase = phase_of(week, season_type, cfp))]
  feats <- feats[margin != 0]   # football ties don't exist after 1996; guards bad rows
  mterms <- CFG$margin_terms
  mcomp <- unname(MARGIN_COMPONENTS[mterms])

  # prior weights: the margin components' k jointly by margin error, then
  # kE and kN by total error (with kP fixed)
  snap <- function(k) K_GRID[which.min(abs(log(K_GRID) - log(k)))]
  choose_k <- function(train, ebk = NULL) {
    vv <- c(paste0("f", COMPONENTS), paste0("t", COMPONENTS))
    wide <- dcast(train, game_id + season + margin + total + loc + trav + phase + fbs_game + lvl + qbd + gp_h + gp_a ~ k, value.var = vv)
    sel <- if (CFG$k_obj == "fbs") wide$fbs_game %in% TRUE else rep(TRUE, nrow(wide))
    w <- layer_weights(wide)
    best <- NULL
    # c3.3: one prior weight for every component. "k_fixed" pins it; k_mode = "uniform" picks it by margin error.
    # The per-component grid search is flat (E, S and P overlap) and its picks drift to the grid edge.
    if (is.finite(CFG$k_fixed %||% NA)) {
      kf <- snap(CFG$k_fixed)
      return(as.list(setNames(rep(kf, length(COMPONENTS)), paste0("k", COMPONENTS))))
    }
    if (identical(CFG$k_mode, "uniform")) {
      mse <- sapply(K_GRID, function(kv) {
        d <- wide[, .(margin, loc, trav, fbs_game, lvl, qbd, gp_h, gp_a, season)]
        for (j in seq_along(mterms)) d[, (mterms[j]) := wide[[paste0(mterms[j], "_", kv)]]]
        f <- lm.wfit(margin_design(d)[sel, , drop = FALSE], d$margin[sel], w[sel])
        sum(w[sel] * f$residuals^2) / sum(w[sel])
      })
      return(as.list(setNames(rep(K_GRID[which.min(mse)], length(COMPONENTS)), paste0("k", COMPONENTS))))
    }
    if (CFG$k_mode == "eb" && !is.null(ebk)) {
      best <- as.list(sapply(COMPONENTS, function(c) snap(ebk[[c]])))
      names(best) <- paste0("k", COMPONENTS)
      return(best)
    }
    combos <- do.call(CJ, setNames(rep(list(K_GRID), length(mcomp)), paste0("k", mcomp)))
    for (i in seq_len(nrow(combos))) {
      d <- wide[, .(margin, loc, trav, fbs_game, lvl, qbd, gp_h, gp_a, season)]
      for (j in seq_along(mterms)) d[, (mterms[j]) := wide[[paste0(mterms[j], "_", combos[[j]][i])]]]
      M <- margin_design(d)
      f <- lm.wfit(M[sel, , drop = FALSE], d$margin[sel], w[sel])
      mse <- sum(w[sel] * f$residuals^2) / sum(w[sel])
      if (is.null(best) || mse < best$mse) best <- c(list(mse = mse), as.list(combos[i]))
    }
    kP <- if (!is.null(best$kP)) best$kP else 4
    tm <- CJ(kE = K_GRID, kN = if (isTRUE(CFG$pace_total)) K_GRID else K_GRID[1])
    tm[, mse := mapply(function(kE, kN) {
      d <- wide[, .(total, season, tE = get(paste0("tE_", kE)), tP = get(paste0("tP_", kP)), tN = get(paste0("tN_", kN)))]
      f <- lm.wfit(total_design(d), d$total, layer_weights(d))
      sum(layer_weights(d) * f$residuals^2) / sum(layer_weights(d))
    }, kE, kN)]
    best$kE <- tm$kE[which.min(tm$mse)]; best$kN <- tm$kN[which.min(tm$mse)]
    for (c in COMPONENTS) if (is.null(best[[paste0("k", c)]])) best[[paste0("k", c)]] <- 4
    best
  }
  pick <- function(d, kk) {
    out <- d[k == kk$kE, .(game_id, season, week, season_type, phase, loc, trav, cfp, margin, total, spread, over_under,
                           fbs_game, lvl, qbd, home_id, away_id, gp_h, gp_a, fE, tE)]
    for (c in setdiff(COMPONENTS, "E")) {
      cols <- c("game_id", paste0("f", c), paste0("t", c))
      out <- merge(out, d[k == kk[[paste0("k", c)]], ..cols], by = "game_id")
    }
    out
  }

  bt <- list(); folds <- list()
  for (s in sort(unique(feats$season))) {
    if (s < BACKTEST_FROM) next
    train <- feats[season < s]
    kk <- choose_k(train, cal$ebk[[as.character(s)]])
    L <- fit_pred_layer(pick(train, kk))
    test <- apply_pred_layer(pick(feats[season == s], kk), L)
    bt[[as.character(s)]] <- test
    folds[[as.character(s)]] <- list(k = kk[paste0("k", COMPONENTS)], coef = L$margin, sigma = L$sigma,
                                     ebk = cal$ebk[[as.character(s)]])
  }
  backtest <- rbindlist(bt)

  # --- final settings for the live season: everything before it
  last_s <- max(as.integer(names(cal$pmodels)))
  kk_live <- choose_k(feats, cal$ebk[[as.character(last_s)]])
  L_live <- fit_pred_layer(pick(feats, kk_live))

  list(cal = cal, feats = feats, backtest = backtest, folds = folds,
       k_live = kk_live, layer = L_live)
}

# ------------------------------------------------------------ live season
# Ratings now, pregame predictions for this season's played games (for
# grading), and predictions for upcoming games.
live_season <- function(model, sd_cur, side, current_season, upcoming = NULL) {
  cal <- model$cal
  past <- sort(as.integer(names(cal$finals)))
  # no portal file for this season: the prior can't be served inputs it was trained on,
  # so refit it without them for this build (restored on exit)
  if (isTRUE(CFG$portal) && portal_missing(side, current_season)) {
    CFG$portal <<- FALSE
    on.exit(CFG$portal <<- TRUE, add = TRUE)
    message("  no portal data for ", current_season, ": prior refit without portal terms")
  }
  pm <- fit_prior_model(past[past >= FIRST_SEASON + 2], cal$finals, side, cal$teams_by)
  games <- copy(sd_cur$games)
  # teams: everyone in this season's games plus anyone in the upcoming slate
  teams_s <- season_teams(all_games(sd_cur))
  if (!is.null(upcoming) && nrow(upcoming)) {
    extra <- rbind(upcoming[, .(id = home_id, name = home_name, div = home_div, conf = NA_character_)],
                   upcoming[, .(id = away_id, name = away_name, div = away_div, conf = NA_character_)])
    extra <- unique(extra[!id %in% teams_s$id], by = "id")
    extra[is.na(div) | div != "fbs", div := "fcs"]
    teams_s <- rbind(teams_s, extra, fill = TRUE)
  }
  prior <- make_prior(current_season, cal$finals, side, teams_s, pm)
  hfa <- as.list(hfa_for(cal$hfa_hist, current_season))
  mu_pr <- as.list(colMeans(do.call(rbind, cal$mu_hist[as.character(c(current_season - 1, current_season - 2))])))
  kk <- model$k_live; L <- model$layer
  coords <- side$coords

  # pregame predictions for games already played (walk this season)
  played <- NULL
  if (nrow(games[completed == TRUE])) {
    prior_cache <- list()
    prior_at <- function(sl) {
      if (!isTRUE(CFG$asof_roster)) return(prior)
      key <- as.character(sl)
      if (is.null(prior_cache[[key]])) prior_cache[[key]] <<- make_prior(current_season, cal$finals, side, teams_s, pm, slot = sl)
      prior_cache[[key]]
    }
    w <- season_walk(sd_cur, prior_at, hfa, teams_s, k_grid = unique(unlist(kk[paste0("k", COMPONENTS)])), mu_by_cmp = mu_pr)
    f <- w$features
    d <- f[k == kk$kE, .(game_id, gp_h, gp_a, fE, tE)]
    for (c in setdiff(COMPONENTS, "E")) {
      cols <- c("game_id", paste0("f", c), paste0("t", c))
      d <- merge(d, f[k == kk[[paste0("k", c)]], ..cols], by = "game_id")
    }
    d <- merge(d, games, by = "game_id")
    d <- merge(d, layer_extras(setNames(list(sd_cur), as.character(current_season)), side, current_season), by = "game_id", all.x = TRUE)
    d[, `:=`(loc = fifelse(neutral, 0, 1), phase = phase_of(week, season_type, cfp),
             fbs_game = home_div == "fbs" & away_div == "fbs", trav = travel_term(d, coords))]
    played <- apply_pred_layer(d, L)
  }

  # ratings through every completed game
  team_ids <- teams_s$id; is_fcs <- teams_s$div == "fcs"
  pr <- prior[match(team_ids, prior$id)]
  fits <- list()
  for (cmp in COMPONENTS) {
    k <- kk[[paste0("k", cmp)]]
    o <- make_obs(sd_cur, games[completed == TRUE], cmp)
    aw <- if (nrow(o)) mean(o$w) else c(E = 62, S = 62, P = 1, R = 12, N = 1)[[cmp]]
    lam <- team_lambda(k, aw, is_fcs)
    fits[[cmp]] <- fit_ratings(o, team_ids, pr[[paste0(cmp, "_o")]], pr[[paste0(cmp, "_d")]],
                               lam * pr[[paste0("w_", cmp, "_o")]], hfa = hfa[[cmp]],
                               mu_prior = mu_pr[[cmp]], mu_lambda = MU_K * aw,
                               lambda_d = lam * pr[[paste0("w_", cmp, "_d")]], huber = huber_for(cmp))
  }
  # preseason-only ratings (prior alone), to show how far each team has moved
  pre <- list()
  for (cmp in COMPONENTS) pre[[cmp]] <- list(O = pr[[paste0(cmp, "_o")]], D = pr[[paste0(cmp, "_d")]])

  # season progress for the rating scale: FBS teams' average games played
  gpl <- rbind(games[completed == TRUE, .(id = home_id)], games[completed == TRUE, .(id = away_id)])[, .N, by = id]
  x_now <- min(mean(gpl$N[match(team_ids[!is_fcs], gpl$id)], na.rm = TRUE), 10) / 10
  if (!is.finite(x_now)) x_now <- 0
  now <- net_scale(fits, L, x_now); pre_s <- net_scale(pre, L, 0)
  fbs <- teams_s$div == "fbs"
  avg_pts <- mean(c(games[completed == TRUE & home_div == "fbs" & away_div == "fbs", c(home_pts, away_pts)]), na.rm = TRUE)
  if (!is.finite(avg_pts)) avg_pts <- 28
  ratings <- data.table(
    id = team_ids, div = teams_s$div,
    off = avg_pts + now$off - mean(now$off[fbs]),
    def = avg_pts + now$def - mean(now$def[fbs]),
    pre_net = (pre_s$off - pre_s$def) - mean((pre_s$off - pre_s$def)[fbs]),
    adj_epa_o = fits$E$mu + fits$E$O, adj_epa_d = fits$E$mu + fits$E$D,
    adj_sr_o = fits$S$mu + fits$S$O, adj_sr_d = fits$S$mu + fits$S$D,
    adj_pts_o = fits$P$mu + fits$P$O, adj_pts_d = fits$P$mu + fits$P$D,
    adj_pace = fits$N$mu / 2 + (fits$N$O + fits$N$D) / 2)
  ratings[, net := off - def]

  # c3.3: the QB each team is expected to start next, against the QBs behind its ratings so far
  qd <- rep(0, length(team_ids)); qb_live <- NULL
  if (isTRUE(CFG$qb_avail) && exists("qb_live_table") && !is.null(side$meta[[as.character(current_season)]])) {
    rs <- if (!is.null(side$roster_slot)) side$roster_slot[season == current_season] else NULL
    qb_live <- tryCatch(qb_live_table(sd_cur, side$meta[[as.character(current_season)]], rs, side$avail),
                        error = function(e) { message("  qb availability skipped: ", conditionMessage(e)); NULL })
    if (!is.null(qb_live) && nrow(qb_live)) { ix <- match(qb_live$id, team_ids); ok <- !is.na(ix); qd[ix[ok]] <- qb_live$dq[ok] }
  }
  qd[!is.finite(qd)] <- 0

  # predict any game between two teams on the current ratings
  predict_pair <- function(g) {
    hi <- match(g$home_id, team_ids); ai <- match(g$away_id, team_ids)
    gpv <- gpl$N[match(team_ids, gpl$id)]; gpv[is.na(gpv)] <- 0
    d <- data.table(loc = ifelse(g$neutral, 0, 1), phase = g$phase, trav = travel_term(g, coords),
                    fbs_game = !is_fcs[hi] & !is_fcs[ai], gp_h = gpv[hi], gp_a = gpv[ai],
                    lvl = fifelse(!is_fcs[hi] & is_fcs[ai], 1, fifelse(is_fcs[hi] & !is_fcs[ai], -1, 0)),
                    qbd = qd[hi] - qd[ai])
    for (cmp in COMPONENTS) {
      f <- fits[[cmp]]; net <- f$O - f$D
      d[, (paste0("f", cmp)) := net[hi] - net[ai]]
      d[, (paste0("t", cmp)) := 2 * f$mu + f$O[hi] + f$D[ai] + f$O[ai] + f$D[hi]]
    }
    apply_pred_layer(d, L)
  }
  up <- NULL
  if (!is.null(upcoming) && nrow(upcoming)) {
    up <- copy(upcoming)
    if (!"cfp" %in% names(up)) up[, cfp := FALSE]
    if (!"venue_id" %in% names(up)) up[, venue_id := NA_integer_]
    up[, phase := phase_of(week, season_type, cfp)]
    pp <- predict_pair(up)
    up[, `:=`(pm = pp$pm, pt = pp$pt, p_home = pp$p_home, sig = pp$sig)]
  }
  # matchup tool: per-team points on the margin scale (FBS games and games
  # with an FCS team), plus the total components; the page does the rest
  net_fcs <- 0
  for (t in names(MARGIN_COMPONENTS)) {
    if (!MARGIN_COMPONENTS[[t]] %in% COMPONENTS || !any(c(t, paste0(t, "_fcs")) %in% names(L$margin))) next
    nm <- paste0(t, "_fcs")
    b <- if (nm %in% names(L$margin)) L$margin[[nm]] else if (t %in% names(L$margin)) L$margin[[t]] else 0
    f <- fits[[MARGIN_COMPONENTS[[t]]]]
    net_fcs <- net_fcs + b * (f$O - f$D)
  }
  list(ratings = ratings, teams = teams_s, played = played, upcoming = up, fits = fits, qb_live = qb_live,
       prior = prior, layer = L, k = kk, hfa = hfa, avg_pts = avg_pts, x_now = x_now,
       matchup = list(
         team_ids = team_ids,
         net = now$off - now$def, net_fcs = net_fcs,
         E = list(mu = fits$E$mu, O = fits$E$O, D = fits$E$D),
         P = list(mu = fits$P$mu, O = fits$P$O, D = fits$P$D),
         N = list(mu = fits$N$mu, O = fits$N$O, D = fits$N$D),
         lat = coords$lat[match(team_ids, coords$team_id)], lon = coords$lon[match(team_ids, coords$team_id)],
         qb_dq = qd,
         margin = as.list(L$margin), total = as.list(L$total), sigma = as.list(L$sigma)))
}

# --------------------------------------------------------- report card
score_block <- function(d) {
  d <- d[is.finite(pm)]
  has_line <- d[is.finite(spread)]
  ats <- has_line[margin + spread != 0]
  ats_pick_home <- ats$pm > -ats$spread
  ats_home_cover <- ats$margin > -ats$spread
  pmk <- if (nrow(has_line)) pnorm(-has_line$spread / has_line$sig) else numeric()
  list(
    games = nrow(d),
    su = mean((d$pm > 0) == (d$margin > 0)),
    logloss = -mean(ifelse(d$margin > 0, log(pmax(d$p_home, 1e-6)), log(pmax(1 - d$p_home, 1e-6)))),
    brier = mean((d$p_home - (d$margin > 0))^2),
    mae = mean(abs(d$margin - d$pm)),
    lined = nrow(has_line),
    mae_model_lined = if (nrow(has_line)) mean(abs(has_line$margin - has_line$pm)) else NA,
    mae_market = if (nrow(has_line)) mean(abs(has_line$margin + has_line$spread)) else NA,
    # picks on the same (lined) games for both, so the two are comparable
    su_model_lined = if (nrow(has_line)) mean((has_line$pm > 0) == (has_line$margin > 0)) else NA,
    su_market = if (nrow(has_line)) mean(((-has_line$spread) > 0) == (has_line$margin > 0)) else NA,
    # the line turned into a win chance with the model's own spread
    logloss_market = if (nrow(has_line)) -mean(ifelse(has_line$margin > 0, log(pmax(pmk, 1e-6)), log(pmax(1 - pmk, 1e-6)))) else NA,
    ats_n = nrow(ats),
    ats = if (nrow(ats)) mean(ats_pick_home == ats_home_cover) else NA,
    ats_big = {
      big <- abs(ats$pm + ats$spread) >= 3
      if (sum(big)) mean(ats_pick_home[big] == ats_home_cover[big]) else NA
    },
    ats_big_n = sum(abs(ats$pm + ats$spread) >= 3),
    total_mae = if ("pt" %in% names(d)) mean(abs(d$total - d$pt)) else NA,
    total_mae_market = if (sum(is.finite(d$over_under))) mean(abs(d$total - d$over_under)[is.finite(d$over_under)]) else NA
  )
}

report_card <- function(model, live) {
  bt <- model$backtest
  by_season <- rbindlist(lapply(split(bt, bt$season), function(d) c(list(season = d$season[1]), score_block(d))), fill = TRUE)
  by_phase <- rbindlist(lapply(split(bt, bt$phase), function(d) c(list(phase = d$phase[1]), score_block(d))), fill = TRUE)
  fbs_only <- score_block(bt[fbs_game == TRUE])
  overall <- score_block(bt)
  # calibration: 10 bins of predicted home win chance
  bt[, bin := pmin(9L, as.integer(floor(p_home * 10)))]
  calib <- bt[, .(n = .N, pred = mean(p_home), actual = mean(margin > 0)), by = bin][order(bin)]
  cur <- NULL
  if (!is.null(live$played) && nrow(live$played)) {
    pl <- copy(live$played)
    pl[, `:=`(margin = home_pts - away_pts, total = home_pts + away_pts)]
    cur <- score_block(pl[completed == TRUE])
  }
  g0 <- function(v, nm) if (nm %in% names(v)) v[[nm]] else NA_real_
  folds <- rbindlist(lapply(names(model$folds), function(s) {
    f <- model$folds[[s]]
    data.table(season = as.integer(s), kE = f$k$kE, kS = f$k$kS, kP = f$k$kP, kN = f$k$kN,
               b_loc = g0(f$coef, "loc"), b_trav = g0(f$coef, "trav"),
               bS = g0(f$coef, "fS"), bP = g0(f$coef, "fP"), bS_x = g0(f$coef, "fS_x"), bP_x = g0(f$coef, "fP_x"),
               sig_early = f$sigma[["early"]], sig_mid = f$sigma[["mid"]], sig_post = f$sigma[["post"]])
  }))
  list(by_season = by_season, by_phase = by_phase, overall = overall, fbs_only = fbs_only,
       calib = calib, current = cur, folds = folds,
       live = list(k = model$k_live[paste0("k", COMPONENTS)], margin = as.list(model$layer$margin),
                   total = as.list(model$layer$total), sigma = as.list(model$layer$sigma), hfa = live$hfa,
                   cfg = CFG))
}
