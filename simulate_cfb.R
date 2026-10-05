# =============================================================================
# simulate_cfb.R -- plays out the rest of the season SIM_N times.
#
# Each run draws every team's true strength from its current rating and its
# uncertainty (wide early, narrowing as games are played), then plays every
# remaining regular-season game, each conference's title game, and the
# 12-team playoff. Sourced by build_cfb.R.
#
# Simplifications, all stated on the dashboard:
#   - Title game participants: the top two in conference winning percentage;
#     ties go to more overall wins, then to the stronger team in that run.
#     Real tiebreakers (head-to-head, rotating schedule metrics) aren't modelled.
#   - Playoff selection stands in for the committee with a resume score:
#     strength in that run + SIM_WIN_PTS points per game over .500.
#     The five best-scoring conference champions get automatic bids, the next
#     seven by score get at-large bids, and seeding is straight by score
#     (the rule since 2025). Seeds 1-4 get byes; seeds 5-8 host round one.
#   - Bowl games other than the playoff aren't simulated.
# =============================================================================

SIM_N        <- 10000L
SIM_WIN_PTS  <- 6        # resume score: points of strength one extra win is worth (hand-set)
SIM_NO_CCG   <- c("FBS Independents")
SIM_SEED     <- 2026L
ARMY_NAVY    <- c(349L, 2426L)   # same conference since 2024, but their game isn't a title game

# Conference title games: two teams from the same conference meeting
# December 1-10 (the data's conference-game flag isn't reliable for them).
is_ccg <- function(home_id, away_id, date, conf_of) {
  d <- as.Date(substr(as.character(date), 1, 10))
  hc <- conf_of[as.character(home_id)]; ac <- conf_of[as.character(away_id)]
  !is.na(hc) & !is.na(ac) & hc == ac & !(hc %in% SIM_NO_CCG) &
    format(d, "%m") == "12" & as.integer(format(d, "%d")) <= 10 &
    !(home_id %in% ARMY_NAVY & away_id %in% ARMY_NAVY)
}

# How far each team's true strength may be from its rating, in points.
# s0 = spread of preseason-projection misses (measured in the backtest);
# it shrinks with games played the way the ratings' prior fades.
rating_sd <- function(gp, s0, k) s0 * sqrt(k / (k + gp))

# Spread of preseason misses in points, from the calibration: prior vs.
# season-end ratings for FBS teams, on the margin scale.
prior_miss_sd <- function(cal, layer) {
  d <- rbindlist(lapply(names(cal$priors), function(s) {
    pr <- cal$priors[[s]]; fn <- cal$finals[[s]]
    if (is.null(fn)) return(NULL)
    pr <- pr[div == "fbs"]
    ids <- intersect(pr$id, fn$S$id)
    pp <- pr[match(ids, pr$id)]
    pl <- lapply(setNames(COMPONENTS, COMPONENTS), function(c) list(O = pp[[paste0(c, "_o")]], D = pp[[paste0(c, "_d")]]))
    fl <- lapply(setNames(COMPONENTS, COMPONENTS), function(c) { x <- fn[[c]][match(ids, fn[[c]]$id)]; list(O = x$O, D = x$D) })
    a <- net_scale(pl, layer, 1); b <- net_scale(fl, layer, 1)
    pre <- data.table(id = ids, pre = a$off - a$def)
    fin <- data.table(id = ids, fin = b$off - b$def)
    m <- merge(pre, fin, by = "id")
    m[, `:=`(pre = pre - mean(pre), fin = fin - mean(fin))]
    m
  }))
  sd(d$fin - d$pre)
}

simulate_season <- function(live, games_done, schedule, conf_of, fbs_ids, s0, n = SIM_N, seed = SIM_SEED) {
  set.seed(seed)
  L <- live$layer
  R <- live$ratings
  ids <- R$id; nT <- length(ids)
  idx <- function(x) match(x, ids)
  # games played so far, per team (for rating uncertainty and records)
  gd <- games_done[completed == TRUE]
  res <- rbind(gd[, .(id = home_id, opp = away_id, w = home_pts > away_pts, conf = conf_game)],
               gd[, .(id = away_id, opp = home_id, w = away_pts > home_pts, conf = conf_game)])
  res[, same_conf := conf & conf_of[as.character(id)] == conf_of[as.character(opp)]]
  rec <- res[, .(gp = .N, w = sum(w), l = sum(!w), cw = sum(w & same_conf %in% TRUE), cl = sum(!w & same_conf %in% TRUE)), by = id]
  gp <- rec$gp[match(ids, rec$id)]; gp[is.na(gp)] <- 0
  sdt <- rating_sd(gp, s0, live$k$kP)
  sig_game <- sqrt(max(L$sigma[["mid"]]^2 - 2 * mean(sdt[ids %in% fbs_ids]^2), 9^2))
  Z <- matrix(R$net, n, nT, byrow = TRUE) + matrix(rnorm(n * nT), n, nT) * matrix(sdt, n, nT, byrow = TRUE)

  W0 <- rec$w[match(ids, rec$id)]; W0[is.na(W0)] <- 0
  L0 <- rec$l[match(ids, rec$id)]; L0[is.na(L0)] <- 0
  CW0 <- rec$cw[match(ids, rec$id)]; CW0[is.na(CW0)] <- 0
  CL0 <- rec$cl[match(ids, rec$id)]; CL0[is.na(CL0)] <- 0
  W <- matrix(W0, n, nT, byrow = TRUE); Lm <- matrix(L0, n, nT, byrow = TRUE)
  CW <- matrix(CW0, n, nT, byrow = TRUE); CL <- matrix(CL0, n, nT, byrow = TRUE)

  # title games already on the schedule (both teams known) fix the participants
  sch <- copy(schedule)
  sch[, `:=`(hc = conf_of[as.character(home_id)], ac = conf_of[as.character(away_id)])]
  sch[, ccg := is_ccg(home_id, away_id, kickoff, conf_of)]
  fixed_ccg <- sch[ccg == TRUE]
  sch <- sch[ccg == FALSE & !is.na(idx(home_id)) & !is.na(idx(away_id))]

  play <- function(hi, ai, loc) {   # returns logical matrix n x length(hi): home won
    m <- L$margin[["loc"]] * matrix(loc, n, length(hi), byrow = TRUE) + Z[, hi, drop = FALSE] - Z[, ai, drop = FALSE] +
      matrix(rnorm(n * length(hi), 0, sig_game), n, length(hi))
    m > 0
  }
  if (nrow(sch)) {
    hi <- idx(sch$home_id); ai <- idx(sch$away_id)
    hw <- play(hi, ai, ifelse(sch$neutral, 0, 1))
    same <- (sch$conf_game %in% TRUE) & !is.na(sch$hc) & ((sch$hc == sch$ac) %in% TRUE)
    for (g in seq_len(nrow(sch))) {
      h <- hi[g]; a <- ai[g]; x <- hw[, g]
      W[, h] <- W[, h] + x; Lm[, h] <- Lm[, h] + !x; W[, a] <- W[, a] + !x; Lm[, a] <- Lm[, a] + x
      if (isTRUE(same[g])) { CW[, h] <- CW[, h] + x; CL[, h] <- CL[, h] + !x; CW[, a] <- CW[, a] + !x; CL[, a] <- CL[, a] + x }
    }
  }

  # conference title games
  fbs_i <- which(ids %in% fbs_ids)
  confs <- setdiff(unique(conf_of[as.character(ids[fbs_i])]), c(SIM_NO_CCG, NA))
  champ <- matrix(FALSE, n, nT); in_ccg <- matrix(FALSE, n, nT)
  for (cf in confs) {
    mem <- fbs_i[conf_of[as.character(ids[fbs_i])] == cf]
    if (length(mem) < 2) next
    fx <- fixed_ccg[hc == cf]
    if (nrow(fx)) {
      t1 <- rep(idx(fx$home_id[1]), n); t2 <- rep(idx(fx$away_id[1]), n)
      loc <- 0
    } else {
      pct <- CW[, mem, drop = FALSE] / pmax(CW[, mem, drop = FALSE] + CL[, mem, drop = FALSE], 1)
      key <- pct * 1e4 + W[, mem, drop = FALSE] * 10 + Z[, mem, drop = FALSE] / 100
      o <- t(apply(key, 1, order, decreasing = TRUE))
      t1 <- mem[o[, 1]]; t2 <- mem[o[, 2]]
    }
    m <- Z[cbind(seq_len(n), t1)] - Z[cbind(seq_len(n), t2)] + rnorm(n, 0, sig_game)
    win <- ifelse(m > 0, t1, t2); lose <- ifelse(m > 0, t2, t1)
    in_ccg[cbind(seq_len(n), t1)] <- TRUE; in_ccg[cbind(seq_len(n), t2)] <- TRUE
    champ[cbind(seq_len(n), win)] <- TRUE
    W[cbind(seq_len(n), win)] <- W[cbind(seq_len(n), win)] + 1
    Lm[cbind(seq_len(n), lose)] <- Lm[cbind(seq_len(n), lose)] + 1
  }

  # playoff
  score <- Z + SIM_WIN_PTS * (W - Lm)
  score[, -fbs_i] <- -Inf
  seed <- matrix(NA_integer_, n, nT)
  bracket <- matrix(0L, n, 12)
  for (r in seq_len(n)) {
    sc <- score[r, ]
    ch <- which(champ[r, ])
    auto <- ch[order(sc[ch], decreasing = TRUE)][seq_len(min(5, length(ch)))]
    rest <- setdiff(order(sc, decreasing = TRUE), auto)
    field <- c(auto, rest[seq_len(12 - length(auto))])
    field <- field[order(sc[field], decreasing = TRUE)]
    bracket[r, ] <- field
    seed[r, field] <- seq_len(12)
  }
  pw <- function(a, b, loc) {   # vector of winners (column indexes) for paired vectors a, b
    m <- L$margin[["loc"]] * loc + Z[cbind(seq_len(n), a)] - Z[cbind(seq_len(n), b)] + rnorm(n, 0, sig_game)
    ifelse(m > 0, a, b)
  }
  B <- function(s) bracket[, s]
  r1 <- list(pw(B(5), B(12), 1), pw(B(6), B(11), 1), pw(B(7), B(10), 1), pw(B(8), B(9), 1))
  qf <- list(pw(B(1), r1[[4]], 0), pw(B(2), r1[[3]], 0), pw(B(3), r1[[2]], 0), pw(B(4), r1[[1]], 0))
  sf <- list(pw(qf[[1]], qf[[4]], 0), pw(qf[[2]], qf[[3]], 0))
  ch_ <- pw(sf[[1]], sf[[2]], 0)
  cnt <- function(v) tabulate(unlist(v), nbins = nT) / n
  qf_teams <- cbind(B(1), B(2), B(3), B(4), r1[[1]], r1[[2]], r1[[3]], r1[[4]])

  out <- data.table(
    id = ids, w_mean = colMeans(W), l_mean = colMeans(Lm), cw_mean = colMeans(CW), cl_mean = colMeans(CL),
    p_bowl = colMeans(W >= 6), p_ccg = colMeans(in_ccg), p_conf = colMeans(champ),
    p_cfp = colMeans(!is.na(seed)), p_bye = colMeans(!is.na(seed) & seed <= 4),
    p_qf = tabulate(as.vector(qf_teams), nbins = nT) / n, p_sf = cnt(qf), p_final = cnt(sf), p_title = cnt(list(ch_)),
    seed_mean = apply(seed, 2, function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)),
    rating_sd = sdt)
  out <- out[id %in% fbs_ids]
  attr(out, "sig_game") <- sig_game
  out
}
