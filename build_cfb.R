# =============================================================================
# build_cfb.R
#
# Rebuilds the FBS college football dashboard (cfb_dashboard.html) from
# real play-by-play, the same way build_dashboards.R does for WBB:
#
#   - play-by-play          : cfbfastR's pbp release (the exact file
#                             cfbfastR::load_cfb_pbp() reads), 2014 onward
#   - final scores          : cfbfastR's ESPN schedule release
#                             (load_espn_cfb_schedules())
#   - team talent composite : load_cfb_team_talent() release
#   - returning production  : load_cfb_returning_production() release
#   - upcoming games        : ESPN's public scoreboard API (no key), with
#                             CollegeFootballData as a fallback when a
#                             CFBD_API_KEY is set, for CFBD_SEASONS only
#
# Like the WBB script this goes straight to the sportsdataverse release
# files instead of through cfbfastR's R functions, so a change in the
# package's exported function surface can't break the build. The files
# are identical to what cfbfastR returns. None of them need an API key.
#
# Past seasons are downloaded once and cached in .cfb_cache/. The current
# season is re-downloaded when the cached copy is more than
# REFRESH_HOURS old (the release updates nightly upstream).
#
# Output: cfb_dashboard.html next to this script. Open it in a browser;
# nothing is fetched at view time except team logos and web fonts.
# =============================================================================

invisible(for (.l in c("en_US.UTF-8", "C.UTF-8", "English_United States.utf8", ".UTF-8"))
  if (isTRUE(l10n_info()$`UTF-8`) || nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", .l)))) break)

needed_pkgs <- c("data.table", "jsonlite", "nanoparquet", "Matrix")
missing_pkgs <- needed_pkgs[!vapply(needed_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  if (!is.null(sys.frames()[[1]]$ofile)) return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
  getwd()
}

# ------------------------------------------------------------------ settings
SCRIPT_DIR    <- get_script_dir()
CACHE_DIR     <- file.path(SCRIPT_DIR, ".cfb_cache")
TEMPLATE_FILE <- file.path(SCRIPT_DIR, "cfb_template.html")
OUT_FILE      <- file.path(SCRIPT_DIR, "cfb_dashboard.html")
PRED_LOG_FILE <- file.path(SCRIPT_DIR, "predictions_log_cfb.csv")

# Season being shown. Defaults to the calendar year from August on.
CFB_SEASON    <- if (as.integer(format(Sys.Date(), "%m")) >= 8) as.integer(format(Sys.Date(), "%Y")) else as.integer(format(Sys.Date(), "%Y")) - 1L
FIRST_SEASON  <- 2014L       # first season with cfbfastR EPA play-by-play
REFRESH_HOURS <- 6           # re-download the current season if older than this
PREP_VERSION  <- "c3.2"      # bump to force every season to be re-processed
UPCOMING_DAYS <- 9           # how far ahead the weekly slate looks
TODAY         <- Sys.Date()  # the build's "today" (override to replay a past date)

# CollegeFootballData API: only these seasons may ever be requested with
# your key. Everything else (all history, the backtest, calibration) comes
# from the public release files and never touches the API. Add a season
# here when you want the key used for it.
CFBD_SEASONS  <- c(2025L, 2026L)
cfbd_calls    <- 0L

# TRUE only when a key is set, cfbfastR is installed and the season is allowed.
cfbd_ok <- function(season) {
  nzchar(Sys.getenv("CFBD_API_KEY")) && requireNamespace("cfbfastR", quietly = TRUE) &&
    all(as.integer(season) %in% CFBD_SEASONS)
}
# Every API call goes through here, so each one is logged and counted.
cfbd_call <- function(what, season, expr) {
  if (!cfbd_ok(season)) return(NULL)
  cfbd_calls <<- cfbd_calls + 1L
  message("  CollegeFootballData call ", cfbd_calls, ": ", what, " ", season)
  tryCatch(as.data.table(expr), error = function(e) { message("    failed: ", conditionMessage(e)); NULL })
}

RELEASE <- "https://github.com/sportsdataverse/sportsdataverse-data/releases/download"
url_pbp      <- function(s) sprintf("%s/cfbfastR_cfb_pbp/play_by_play_%d.rds", RELEASE, s)
url_sched    <- function(s) sprintf("%s/espn_cfb_schedules/cfb_schedule_%d.rds", RELEASE, s)
url_talent   <- function(s) sprintf("%s/cfb_team_talent/cfb_team_talent_%d.parquet", RELEASE, s)
url_returning<- function(s) sprintf("%s/cfb_returning_production/cfb_returning_production_%d.parquet", RELEASE, s)
# CollegeFootballData schedules: every division (FCS-vs-FCS scores), venues, playoff rounds
url_cfbd_sched <- function(s) sprintf("%s/cfb_schedules/cfb_schedules_%d.parquet", RELEASE, s)
# team-level transfer portal summary (incoming and outgoing transfers, 247 ratings)
url_portal   <- function(s) sprintf("%s/cfb_team_portal/cfb_team_portal_%d.parquet", RELEASE, s)
# team info: home stadium coordinates, for travel distance
url_team_info <- function(s) sprintf("%s/cfb_team_info/cfb_team_info_%d.parquet", RELEASE, s)

dir.create(CACHE_DIR, showWarnings = FALSE)
options(timeout = max(600, getOption("timeout")))

# ------------------------------------------------------------- downloading
# Downloads url to the cache unless a fresh enough copy is there.
# Returns the local path, or NULL if the file doesn't exist upstream.
cached_download <- function(url, max_age_hours = Inf) {
  dest <- file.path(CACHE_DIR, basename(url))
  if (file.exists(dest)) {
    age <- as.numeric(difftime(Sys.time(), file.mtime(dest), units = "hours"))
    if (age < max_age_hours) return(dest)
  }
  tmp <- paste0(dest, ".part")
  ok <- tryCatch({
    utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
    file.size(tmp) > 1000
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (isTRUE(ok)) { file.rename(tmp, dest); return(dest) }
  unlink(tmp)
  if (file.exists(dest)) {
    message("  could not refresh ", basename(url), "; using the cached copy")
    return(dest)
  }
  NULL
}

read_release <- function(url, max_age_hours = Inf) {
  path <- cached_download(url, max_age_hours)
  if (is.null(path)) return(NULL)
  out <- tryCatch(
    if (grepl("\\.parquet$", path)) nanoparquet::read_parquet(path) else readRDS(path),
    error = function(e) { message("  unreadable: ", basename(path), " (", conditionMessage(e), ")"); NULL })
  if (is.null(out)) return(NULL)
  as.data.table(as.data.frame(out))
}


# ------------------------------------------------- credits named in the text
# Some seasons' play-by-play names a defender only in the play text ("broken up
# by #14 T.Martin", "forced by #15 A.Howard"), with no player id: 2026 so far has
# no breakup ids at all. These helpers pull the jersey and name out of the text
# so the build can match them to a player. On 2025, where most plays carry both,
# the match agreed with the id on every play it could check.
tc_norm <- function(x) { x <- tolower(x); x <- gsub("\\b(jr|sr|ii|iii|iv|v)\\b\\.?", "", x); gsub("[^a-z]", "", x) }
TC_NAME <- "(#[0-9]+ [A-Za-z'\\-]+\\.[A-Za-z'\\-]+( (Jr|Sr|II|III|IV)\\.?)?|[A-Za-z'\\-\\. ]+,[A-Za-z'\\-]+|[A-Z][a-z'\\-]+ [A-Z][A-Za-z'\\-]+)"
tc_grab <- function(txt, lead) {
  txt <- ifelse(is.na(txt), "", txt)
  rx <- paste0(lead, TC_NAME); at <- regexpr(rx, txt)
  out <- rep(NA_character_, length(txt))
  out[at > 0] <- sub(lead, "", regmatches(txt, at))
  out
}
# "#14 T.Martin", "#35 S.Soles Jr.", "Martin,Tre" or "Tre Martin" -> jersey, first initial, last name
tc_parse <- function(x) {
  x <- trimws(ifelse(is.na(x), "", x))
  jer <- suppressWarnings(as.integer(ifelse(grepl("^#[0-9]+ ", x), sub("^#([0-9]+) .*$", "\\1", x), NA)))
  x <- sub("^#[0-9]+ ", "", x)
  first <- last <- rep(NA_character_, length(x))
  a <- grepl("^[A-Za-z'\\-]+\\.[A-Za-z]", x)
  first[a] <- sub("^([A-Za-z'\\-]+)\\..*$", "\\1", x[a]); last[a] <- sub("^[A-Za-z'\\-]+\\.", "", x[a])
  b <- !a & grepl("^[^,]+,[A-Za-z]", x)
  last[b] <- sub(",.*$", "", x[b]); first[b] <- sub("^[^,]+,", "", x[b])
  c_ <- !a & !b & grepl(" ", x)
  first[c_] <- sub(" .*$", "", x[c_]); last[c_] <- sub("^[^ ]+ ", "", x[c_])
  data.table(jersey = jer, ini = substr(tc_norm(first), 1, 1), lastn = tc_norm(last))
}

# ------------------------------------------------------------- season prep
# Garbage time, Bill Connelly's definition: a play is garbage time when the
# score margin at the snap is more than 43 (Q1), 37 (Q2), 27 (Q3) or 21 (Q4).
# Overtime is never garbage time.
GARBAGE_MARGIN <- c(43, 37, 27, 21)

num <- function(x) suppressWarnings(as.numeric(x))
flag <- function(x) { x <- num(x); !is.na(x) & x == 1 }

# ESPN athlete ids for the passer (qb_i), ball carrier (rb_i) and target (tg_i)
# on each scrimmage play, added to s by reference. s needs game_id, off_id and the
# flags is_pass, is_rush, sk, ptd, rtd, it. Shared by prep_season() and the game
# recaps (recap_cfb.R) so both count the same player on the same play.
box_player_ids <- function(s) {
  idc <- function(dt, col) suppressWarnings(as.integer(as.character(if (col %in% names(dt)) dt[[col]] else NA)))
  s[, `:=`(qb_i = fcoalesce(idc(s, "completion_player_id"), idc(s, "incompletion_player_id"),
                            idc(s, "sack_taken_player_id"), idc(s, "interception_thrown_player_id")),
           rb_i = idc(s, "rush_player_id"),
           tg_i = fcoalesce(idc(s, "reception_player_id"), idc(s, "target_player_id")),
           td_i = idc(s, "touchdown_player_id"))]
  # scoring plays sometimes carry only the touchdown scorer's id
  s[is.na(tg_i) & ptd & !it, tg_i := td_i]
  s[is.na(rb_i) & rtd & is_rush, rb_i := td_i]
  # about 9% of pass plays have no passer id (and some runs and catches no player
  # id) but do have the name parsed from the play text; fill the id from other
  # plays in the same game where that name and an id appear together
  fill_id <- function(idcol, namecol, rows) {
    if (!namecol %in% names(s)) return(invisible())
    s[, nm_tmp := as.character(get(namecol))]
    mp <- s[rows & !is.na(get(idcol)) & !is.na(nm_tmp), .N, by = c("game_id", "off_id", "nm_tmp", idcol)]
    setorder(mp, -N); mp <- unique(mp, by = c("game_id", "off_id", "nm_tmp"))
    s[mp, on = c("game_id", "off_id", "nm_tmp"), fid := get(paste0("i.", idcol))]
    s[rows & is.na(get(idcol)) & !is.na(fid), (idcol) := fid]
    s[, c("nm_tmp", "fid") := NULL]
  }
  s[, fid := NA_integer_]
  fill_id("qb_i", "passer_player_name", s$is_pass)
  s[, fid := NA_integer_]
  fill_id("rb_i", "rusher_player_name", s$is_rush)
  s[, fid := NA_integer_]
  fill_id("tg_i", "receiver_player_name", s$is_pass & !s$sk)
  invisible(s)
}

# Turns one season of play-by-play (plus the ESPN schedule for final scores)
# into four compact tables: games, team-games, drives, and a small play table.
prep_season <- function(season) {
  is_current <- season >= CFB_SEASON
  max_age <- if (is_current) REFRESH_HOURS else Inf
  pbp_path <- cached_download(url_pbp(season), max_age)
  if (is.null(pbp_path)) return(NULL)

  out_path <- file.path(CACHE_DIR, sprintf("prep_%d_%s.rds", season, PREP_VERSION))
  if (file.exists(out_path) && file.mtime(out_path) >= file.mtime(pbp_path)) return(readRDS(out_path))

  message("  processing ", season, " play-by-play")
  p <- as.data.table(readRDS(pbp_path))

  # ---- games (one row each), only those with at least one FBS team
  gcols <- c("game_id", "season", "week", "season_type", "start_date", "home", "away",
             "home_team_id", "away_team_id", "home_team_division", "away_team_division",
             "home_team_conference", "away_team_conference", "neutral_site", "conference_game",
             "spread", "over_under", "provider")
  games <- unique(p[, ..gcols], by = "game_id")
  setnames(games, c("home", "away", "home_team_id", "away_team_id", "home_team_division",
                    "away_team_division", "home_team_conference", "away_team_conference",
                    "neutral_site", "conference_game"),
           c("home_name", "away_name", "home_id", "away_id", "home_div", "away_div",
             "home_conf", "away_conf", "neutral", "conf_game"))
  games[, `:=`(season = as.integer(season), week = as.integer(week),
               home_id = as.integer(home_id), away_id = as.integer(away_id),
               neutral = as.logical(neutral) %in% TRUE, conf_game = as.logical(conf_game) %in% TRUE,
               spread = num(spread), over_under = num(over_under),
               game_date = as.Date(substr(start_date, 1, 10)))]
  games[is.na(home_div), home_div := "other"]; games[is.na(away_div), away_div := "other"]
  games <- games[home_div == "fbs" | away_div == "fbs"]
  games <- games[!is.na(home_id) & !is.na(away_id)]

  # final scores: ESPN schedule release first, the play-by-play as fallback
  sched <- read_release(url_sched(season), max_age)
  if (!is.null(sched) && nrow(sched)) {
    sched <- sched[, .(game_id = as.integer(game_id), s_home = num(home_score), s_away = num(away_score),
                       s_home_id = as.integer(home_id), status, tipoff = as.character(game_date))]
    games[, game_id := as.integer(game_id)]
    games <- merge(games, sched, by = "game_id", all.x = TRUE)
    # ESPN occasionally lists home/away the other way round from CFBD
    swap <- games[, !is.na(s_home_id) & s_home_id != home_id]
    games[swap, `:=`(s_home = s_away, s_away = s_home)]
  } else {
    games[, game_id := as.integer(game_id)]
    games[, `:=`(s_home = NA_real_, s_away = NA_real_, status = NA_character_, tipoff = NA_character_)]
  }
  p[, game_id := as.integer(game_id)]
  p <- p[game_id %in% games$game_id]

  # name -> id inside each game (pos_team is a school name)
  idmap <- rbind(games[, .(game_id, name = home_name, id = home_id)],
                 games[, .(game_id, name = away_name, id = away_id)])
  p <- merge(p, idmap[, .(game_id, pos_team = name, off_id = id)], by = c("game_id", "pos_team"), all.x = TRUE)
  p <- merge(p, idmap[, .(game_id, def_pos_team = name, def_id = id)], by = c("game_id", "def_pos_team"), all.x = TRUE)

  # pbp fallback for scores: the larger score each side ever reached
  # (a final scoring play can be missed; the ESPN schedule score is used first)
  pb_scores <- p[!is.na(off_id), .(pts = max(c(num(pos_team_score), 0), na.rm = TRUE)), by = .(game_id, team = off_id)]
  pb_scores2 <- p[!is.na(def_id), .(pts = max(c(num(def_pos_team_score), 0), na.rm = TRUE)), by = .(game_id, team = def_id)]
  pb_scores <- rbind(pb_scores, pb_scores2)[, .(pts = max(pts)), by = .(game_id, team)]
  games <- merge(games, pb_scores[, .(game_id, home_id = team, pb_home = pts)], by = c("game_id", "home_id"), all.x = TRUE)
  games <- merge(games, pb_scores[, .(game_id, away_id = team, pb_away = pts)], by = c("game_id", "away_id"), all.x = TRUE)
  games[, `:=`(home_pts = fifelse(!is.na(s_home), s_home, pb_home),
               away_pts = fifelse(!is.na(s_away), s_away, pb_away))]
  games[, completed := !is.na(home_pts) & !is.na(away_pts) & (is.na(status) | status == "STATUS_FINAL")]

  # ---- scrimmage plays
  x <- p[(flag(rush) | flag(pass)) & !is.na(off_id) & !is.na(def_id) & !is.na(num(EPA))]
  x[, `:=`(period = as.integer(period), sdiff = num(score_diff_start), EPA = num(EPA),
           success = flag(success), is_rush = flag(rush), is_pass = flag(pass),
           yds = num(yards_gained), sack = flag(sack_vec), int = flag(int),
           to = flag(turnover_vec))]
  x[, garbage := !is.na(sdiff) & period <= 4 & abs(sdiff) > GARBAGE_MARGIN[pmin(pmax(period, 1L), 4L)]]
  x[, `:=`(
    tfl   = is_rush & !is.na(yds) & yds < 0,
    pbu   = !is.na(pass_breakup_player_name) & nzchar(pass_breakup_player_name),
    ff    = !is.na(fumble_forced_player_name) & nzchar(fumble_forced_player_name),
    expl  = !is.na(yds) & yds >= 20
  )]
  x[, havoc := tfl | sack | int | pbu | ff]

  agg <- function(d) d[, .(
    plays = .N, epa = sum(EPA), succ = sum(success),
    rush_n = sum(is_rush), rush_epa = sum(EPA[is_rush]), rush_succ = sum(success[is_rush]),
    pass_n = sum(is_pass), pass_epa = sum(EPA[is_pass]), pass_succ = sum(success[is_pass]),
    succ_epa = sum(EPA[success]), expl = sum(expl), havoc = sum(havoc),
    sacks = sum(sack), to = sum(to), yds = sum(yds, na.rm = TRUE)
  ), by = .(game_id, off_id, def_id)]
  tg_all <- agg(x)
  tg_clean <- agg(x[garbage == FALSE])

  # ---- drives (field position, finishing drives)
  d <- p[!is.na(off_id) & !is.na(drive_id), .(
    start_ytg = num(drive_start_yards_to_goal)[1],
    pts = num(drive_pts)[1],
    min_ytg = suppressWarnings(min(num(yards_to_goal)[flag(rush) | flag(pass)], na.rm = TRUE)),
    sdiff0 = num(score_diff_start)[1], period0 = as.integer(period)[1],
    result = drive_result_detailed[1],
    secs = num(drive_time_minutes_elapsed)[1] * 60 + num(drive_time_seconds_elapsed)[1]
  ), by = .(game_id, drive_id, off_id)]
  d <- d[!is.na(start_ytg) & start_ytg > 0 & start_ytg < 100]
  d[!is.finite(min_ytg), min_ytg := start_ytg]
  d[is.na(pts), pts := 0]
  # a drive counts if it had a scrimmage play; kneel-downs at the half/end drop out
  d <- d[!(result %in% c("End Half", "End of Half", "End of Game", "End of 4th Quarter"))]
  d[, garbage := !is.na(sdiff0) & period0 <= 4 & abs(sdiff0) > GARBAGE_MARGIN[pmin(pmax(period0, 1L), 4L)]]
  d[, scoring_opp := min_ytg <= 40]
  d[, reg := !is.na(period0) & period0 <= 4]
  # drives_reg / drive_pts_reg: regulation only (overtime possessions start at the 25
  # and would inflate points per drive); these feed the points-per-drive rating
  dg <- d[garbage == FALSE, .(drives = .N, drive_pts = sum(pmax(pts, 0)), start_ytg_sum = sum(start_ytg),
                              opps = sum(scoring_opp), opp_pts = sum(pmax(pts, 0)[scoring_opp]),
                              drives_reg = sum(reg), drive_pts_reg = sum(pmax(pts, 0)[reg])),
          by = .(game_id, off_id)]
  tg_clean <- merge(tg_clean, dg, by = c("game_id", "off_id"), all.x = TRUE)
  for (cc in c("drives", "drive_pts", "start_ytg_sum", "opps", "opp_pts", "drives_reg", "drive_pts_reg"))
    tg_clean[is.na(get(cc)), (cc) := 0]

  # ---- per-player lines (kept small; used by the team pages' leaders)
  # ESPN participant columns carry clean names (passer_player_name is
  # parsed from play text and sometimes picks up formation words)
  chr <- function(col) if (col %in% names(x)) as.character(x[[col]]) else rep(NA_character_, nrow(x))
  x[, `:=`(qb = fcoalesce(chr("completion_player"), chr("incompletion_player"), chr("sack_taken_player"),
                          chr("interception_thrown_player")),
           rb = chr("rush_player"),
           wr = fcoalesce(chr("reception_player"), chr("target_player")))]
  pl_pass <- x[garbage == FALSE & is_pass & !is.na(qb) & nzchar(qb),
               .(n = .N, epa = sum(EPA), succ = sum(success)), by = .(game_id, team = off_id, player = qb)][, role := "pass"]
  pl_rush <- x[garbage == FALSE & is_rush & !is.na(rb) & nzchar(rb),
               .(n = .N, epa = sum(EPA), succ = sum(success)), by = .(game_id, team = off_id, player = rb)][, role := "rush"]
  pl_rec  <- x[garbage == FALSE & is_pass & !sack & !is.na(wr) & nzchar(wr),
               .(n = .N, epa = sum(EPA), succ = sum(success)), by = .(game_id, team = off_id, player = wr)][, role := "rec"]
  players <- rbind(pl_pass, pl_rush, pl_rec)

  # ---- play-level table for the player model (garbage time removed)
  int_id <- function(col) suppressWarnings(as.integer(as.character(if (col %in% names(x)) x[[col]] else NA)))
  pos_of <- function(col) if (col %in% names(x)) as.character(x[[col]]) else rep(NA_character_, nrow(x))
  x[, `:=`(
    qb_id = fcoalesce(int_id("completion_player_id"), int_id("incompletion_player_id"),
                      int_id("sack_taken_player_id"), int_id("interception_thrown_player_id")),
    qb_pos = fcoalesce(pos_of("position_completion"), pos_of("position_incompletion"),
                       pos_of("position_sack_taken"), pos_of("position_interception_thrown")),
    rb_id = int_id("rush_player_id"), rb_pos = pos_of("position_rush"),
    wr_id = fcoalesce(int_id("reception_player_id"), int_id("target_player_id")),
    wr_pos = fcoalesce(pos_of("position_reception"), pos_of("position_target")))]
  plays <- x[garbage == FALSE, .(game_id, off_id, def_id, EPA, success, is_pass, is_rush, sack,
                                 qb_id = fifelse(is_pass, qb_id, NA_integer_), qb, qb_pos,
                                 rb_id = fifelse(is_rush, rb_id, NA_integer_), rb, rb_pos,
                                 wr_id = fifelse(is_pass & !sack, wr_id, NA_integer_), wr, wr_pos)]

  # ---- defensive playmaking counts (attribution is patchy before 2016 and in some seasons)
  dp <- function(idc, namec, posc, what) {
    if (!all(c(idc, namec) %in% names(p))) return(NULL)
    d <- p[!is.na(def_id) & !is.na(get(idc)) & (flag(rush) | flag(pass)),
           .(id = suppressWarnings(as.integer(as.character(get(idc)))), name = as.character(get(namec)),
             pos = if (posc %in% names(p)) as.character(get(posc)) else NA_character_, team = def_id, game_id)]
    d[, what := what]
    d
  }
  defp <- rbind(dp("sack_player_id", "sack_player", "position_sack", "sack"),
                dp("interception_player_id", "interception_player", "position_interception", "int"),
                dp("pass_breakup_player_id", "pass_breakup_player", "position_pass_breakup", "pbu"),
                dp("fumble_forced_player_id", "fumble_forced_player", "position_fumble_forced", "ff"), fill = TRUE)
  if (!is.null(defp) && nrow(defp)) defp <- defp[!is.na(id), .N, by = .(id, name, pos, team, what)]

  # ---- traditional box scores (c3.1). Every scrimmage play counts, garbage time
  # included, the way box scores count. Sacks count against passing (NFL style;
  # the NCAA counts sack yards as rushing). Penalties aren't attributed to a team
  # in the play-by-play, so there are no penalty totals.
  s <- p[(flag(rush) | flag(pass)) & !is.na(off_id) & !is.na(def_id)]
  s[, `:=`(yds = num(yards_gained), is_pass = flag(pass), is_rush = flag(rush), sk = flag(sack_vec),
           cmp = flag(completion), ptd = flag(pass_td), rtd = flag(rush_td), it = flag(int),
           fl = flag(fumble_vec) & flag(turnover_vec) & !flag(int), dn = suppressWarnings(as.integer(down)),
           dist = num(distance))]
  s[is.na(yds), yds := 0]
  # firstD_by_yards is almost never set in the play-by-play, so a first down is a
  # play that gained the distance without a turnover; a conversion also counts a TD
  s[, fd := !is.na(dist) & dist > 0 & yds >= dist & !it & !fl & !sk]
  s[, conv := fd | ptd | rtd]
  box <- s[, .(plays = .N, yds = sum(yds),
               pass_att = sum(is_pass & !sk), cmp = sum(cmp & is_pass & !sk), pass_yds = sum(yds[is_pass & !sk]),
               pass_td = sum(ptd & is_pass), ints = sum(it), sacks = sum(sk), sack_yds = -sum(yds[sk]),
               rush_att = sum(is_rush), rush_yds = sum(yds[is_rush]), rush_td = sum(rtd & is_rush),
               fum_lost = sum(fl), fd = sum(fd), big = sum(yds >= 20),
               d3 = sum(dn %in% 3L), d3c = sum(dn %in% 3L & conv), d4 = sum(dn %in% 4L), d4c = sum(dn %in% 4L & conv)),
           by = .(game_id, off_id, def_id)]
  pen_fd <- p[!is.na(off_id) & flag(firstD_by_penalty), .(pen_fd = .N), by = .(game_id, off_id)]
  box <- merge(box, pen_fd, by = c("game_id", "off_id"), all.x = TRUE)
  box[is.na(pen_fd), pen_fd := 0L]
  rz <- d[, .(rz = sum(min_ytg <= 20), rz_td = sum(min_ytg <= 20 & pts >= 6), rz_sc = sum(min_ytg <= 20 & pts > 0),
              top = sum(secs, na.rm = TRUE)), by = .(game_id, off_id)]
  box <- merge(box, rz, by = c("game_id", "off_id"), all.x = TRUE)
  # yds_punted is missing on most punts; the distance is in the play text ("punt for 52 yds")
  # (two styles: "punt for 52 yds" and "punt 36 yards")
  p[, punt_dist := { v <- num(yds_punted); rx <- "^.*[Pp]unt (for )?(-?[0-9]+) y.*$"
                     tx <- suppressWarnings(as.numeric(sub(rx, "\\2", play_text)))
                     fifelse(is.finite(v) & v > 0, v, fifelse(grepl(rx, play_text), tx, NA_real_)) }]
  kk <- p[!is.na(off_id), .(fga = sum(flag(fg_inds)), fgm = sum(flag(fg_made)), punts = sum(flag(punt) & !is.na(punt_dist)),
                            punt_yds = sum(punt_dist[flag(punt)], na.rm = TRUE)), by = .(game_id, off_id)]
  box <- merge(box, kk, by = c("game_id", "off_id"), all.x = TRUE)
  for (cc in c("rz", "rz_td", "rz_sc", "top", "fga", "fgm", "punts", "punt_yds")) box[is.na(get(cc)), (cc) := 0]

  # player box lines, one row per player per game (ESPN athlete ids, as in the player model)
  idc <- function(dt, col) suppressWarnings(as.integer(as.character(if (col %in% names(dt)) dt[[col]] else NA)))
  box_player_ids(s)
  mx <- function(v) if (length(v)) max(v) else 0
  pb_pass <- s[is_pass & !is.na(qb_i), .(att = sum(!sk), cmp = sum(cmp & !sk), yds = sum(yds[!sk]), td = sum(ptd),
                                        int = sum(it), sk = sum(sk), lng = mx(yds[cmp & !sk])), by = .(game_id, team = off_id, id = qb_i)]
  pb_rush <- s[is_rush & !is.na(rb_i), .(att = .N, yds = sum(yds), td = sum(rtd), lng = mx(yds)), by = .(game_id, team = off_id, id = rb_i)]
  pb_rec <- s[is_pass & !sk & !is.na(tg_i), .(tgt = .N, rec = sum(cmp), yds = sum(yds[cmp]), td = sum(ptd & cmp),
                                             lng = mx(yds[cmp])), by = .(game_id, team = off_id, id = tg_i)]
  dcount <- function(col, what) {
    v <- idc(s, col); k <- !is.na(v)
    if (!any(k)) return(NULL)
    data.table(game_id = s$game_id[k], team = s$def_id[k], id = v[k], what = what)
  }
  pb_def <- rbind(dcount("sack_player_id", "sack"), dcount("interception_player_id", "int"),
                  dcount("pass_breakup_player_id", "pbu"), dcount("fumble_forced_player_id", "ff"))
  fr <- s[fl == TRUE, .(game_id, team = def_id, id = idc(.SD, "fumble_recovered_player_id"))][!is.na(id)]
  if (nrow(fr)) pb_def <- rbind(pb_def, fr[, what := "fr"])
  if (!is.null(pb_def) && nrow(pb_def)) pb_def <- dcast(pb_def[, .N, by = .(game_id, team, id, what)],
                                                       game_id + team + id ~ what, value.var = "N", fill = 0L)
  # credits named only in the text (no id): kept with jersey and name for the build to match
  txt_s <- as.character(s$play_text); nop <- grepl("NO PLAY", txt_s, fixed = TRUE)
  tcred <- function(lead, idcol, what) {
    k <- which(!nop & is.na(idc(s, idcol)) & grepl(lead, txt_s, fixed = TRUE))
    if (!length(k)) return(NULL)
    g <- tc_grab(txt_s[k], lead)
    out <- data.table(game_id = s$game_id[k], team = s$def_id[k], what = what, label = sub("^#[0-9]+ ", "", g), tc_parse(g))
    out[!is.na(g) & nzchar(lastn)]
  }
  pb_text <- rbind(tcred("broken up by ", "pass_breakup_player_id", "pbu"), tcred("forced by ", "fumble_forced_player_id", "ff"))
  # who's who on each defense: every id credited with a full name, plus the jersey
  # where the play text names the same player
  seen <- function(idcol, ncol, lead) {
    if (!all(c(idcol, ncol) %in% names(s))) return(NULL)
    k <- which(!is.na(idc(s, idcol)))
    if (!length(k)) return(NULL)
    nm <- as.character(s[[ncol]][k])
    pn <- tc_parse(tc_grab(txt_s[k], lead))
    d <- data.table(team = s$def_id[k], id = idc(s, idcol)[k], ini = substr(tc_norm(sub(" .*$", "", nm)), 1, 1),
                    lastn = tc_norm(sub("^[^ ]+ ", "", nm)))
    d[, jersey := fifelse(pn$lastn == lastn, pn$jersey, NA_integer_)]
    d
  }
  def_seen <- rbind(seen("sack_player_id", "sack_player", "\\("), seen("interception_player_id", "interception_player", "intercepted by "),
                    seen("fumble_recovered_player_id", "fumble_recovered_player", "recovered by [A-Za-z ]+ "),
                    seen("pass_breakup_player_id", "pass_breakup_player", "broken up by "),
                    seen("fumble_forced_player_id", "fumble_forced_player", "forced by "))
  if (!is.null(def_seen)) def_seen <- unique(def_seen[!is.na(team) & nzchar(lastn)])

  fgp <- p[flag(fg_inds) & !is.na(off_id)]
  pb_kick <- NULL
  if (nrow(fgp)) {
    # the kicker's id is missing on over half of attempts; his name almost never is,
    # so attempts are grouped by team and name and take whatever id appears
    fgp[, kid := fcoalesce(idc(fgp, "field_goal_made_player_id"), idc(fgp, "field_goal_missed_player_id"),
                           idc(fgp, "field_goal_attempt_player_id"))]
    fgp[, kname := as.character(if ("fg_kicker_player_name" %in% names(fgp)) fg_kicker_player_name else NA)]
    fgp[, `:=`(made = flag(fg_made), dist = num(yds_fg))]
    fgp <- fgp[!is.na(kname) & nzchar(kname)]
    fgp[, kid := { v <- kid[!is.na(kid)]; if (length(v)) as.integer(names(which.max(table(v)))) else NA_integer_ }, by = .(off_id, kname)]
    pb_kick <- fgp[, .(id = kid[1], fga = .N, fgm = sum(made), lng = mx(dist[made & !is.na(dist)]),
                       fg40 = sum(!is.na(dist) & dist >= 40), fgm40 = sum(made & !is.na(dist) & dist >= 40)),
                   by = .(game_id, team = off_id, name = kname)]
  }
  # names and positions for every id that appears
  nmrow <- function(dt, idcol, ncol, pcol) {
    if (!all(c(idcol, ncol) %in% names(dt))) return(NULL)
    data.table(id = idc(dt, idcol), name = as.character(dt[[ncol]]),
               pos = if (pcol %in% names(dt)) as.character(dt[[pcol]]) else NA_character_)[!is.na(id)]
  }
  pnames <- rbind(nmrow(s, "completion_player_id", "completion_player", "position_completion"),
                  nmrow(s, "incompletion_player_id", "incompletion_player", "position_incompletion"),
                  nmrow(s, "sack_taken_player_id", "sack_taken_player", "position_sack_taken"),
                  nmrow(s, "rush_player_id", "rush_player", "position_rush"),
                  nmrow(s, "reception_player_id", "reception_player", "position_reception"),
                  nmrow(s, "target_player_id", "target_player", "position_target"),
                  nmrow(s, "sack_player_id", "sack_player", "position_sack"),
                  nmrow(s, "interception_player_id", "interception_player", "position_interception"),
                  nmrow(s, "pass_breakup_player_id", "pass_breakup_player", "position_pass_breakup"),
                  nmrow(s, "fumble_forced_player_id", "fumble_forced_player", "position_fumble_forced"),
                  nmrow(s, "fumble_recovered_player_id", "fumble_recovered_player", "position_fumble_recovered"),
                  nmrow(fgp, "field_goal_made_player_id", "field_goal_made_player", "position_field_goal_made"),
                  nmrow(fgp, "field_goal_missed_player_id", "field_goal_missed_player", "position_field_goal_missed"), fill = TRUE)
  if (!is.null(pnames) && nrow(pnames)) pnames <- pnames[, .(name = mode1(name), pos = mode1(pos)), by = id]
  pbox <- list(pass = pb_pass, rush = pb_rush, rec = pb_rec, def = pb_def, kick = pb_kick, names = pnames,
               text = pb_text, seen = def_seen)

  # ---- CollegeFootballData schedule: venue, playoff round, and the FCS-vs-FCS games
  # (scores only; they connect FCS teams to each other for the points rating)
  cs <- read_release(url_cfbd_sched(season), max_age)
  games_fcs <- NULL
  games[, `:=`(venue_id = NA_integer_, cfp = FALSE)]
  if (!is.null(cs) && nrow(cs)) {
    cs[, game_id := as.integer(game_id)]
    cs[, cfp_flag := (!is.na(playoff_round) & nzchar(as.character(playoff_round))) | playoff_competition %in% TRUE]
    games[cs, on = "game_id", `:=`(venue_id = as.integer(i.venue_id), cfp = i.cfp_flag %in% TRUE)]
    f <- cs[home_division %in% "fcs" & away_division %in% "fcs" & completed %in% TRUE &
            !is.na(home_points) & !is.na(away_points) & !(game_id %in% games$game_id)]
    if (nrow(f)) games_fcs <- f[, .(game_id, season = as.integer(season), week = as.integer(week),
                                   season_type = as.character(season_type),
                                   game_date = as.Date(substr(as.character(start_date), 1, 10)),
                                   home_id = as.integer(home_id), away_id = as.integer(away_id),
                                   home_name = home_team, away_name = away_team, home_div = "fcs", away_div = "fcs",
                                   home_conf = home_conference, away_conf = away_conference,
                                   neutral = neutral_site %in% TRUE, home_pts = num(home_points), away_pts = num(away_points),
                                   completed = TRUE)]
  }

  keep_g <- c("game_id", "season", "week", "season_type", "game_date", "tipoff", "home_id", "away_id",
              "home_name", "away_name", "home_div", "away_div", "home_conf", "away_conf",
              "neutral", "conf_game", "spread", "over_under", "provider", "home_pts", "away_pts", "completed",
              "venue_id", "cfp")
  res <- list(games = games[, ..keep_g], tg = tg_clean, tg_all = tg_all, players = players,
              plays = plays, defp = defp, games_fcs = games_fcs, box = box, pbox = pbox)
  saveRDS(res, out_path)
  rm(p, x); gc(verbose = FALSE)
  res
}

# ---------------------------------------------------------- side datasets
load_side <- function(seasons) {
  tal <- rbindlist(lapply(seasons, function(s) {
    t <- read_release(url_talent(s), if (s >= CFB_SEASON) REFRESH_HOURS * 4 else Inf)
    if (is.null(t)) return(NULL)
    t[, .(season = as.integer(season), team_id = as.integer(team_id), talent = num(talent_composite))]
  }), fill = TRUE)
  ret <- rbindlist(lapply(seasons, function(s) {
    r <- read_release(url_returning(s), if (s >= CFB_SEASON) REFRESH_HOURS * 4 else Inf)
    if (is.null(r)) return(NULL)
    r[, .(season = as.integer(season), team_id = as.integer(team_id),
          ret_off = num(off_returning), ret_def = num(def_returning))]
  }), fill = TRUE)
  portal <- rbindlist(lapply(seasons, function(s) {
    p <- read_release(url_portal(s), if (s >= CFB_SEASON) REFRESH_HOURS * 4 else Inf)
    if (is.null(p) || !nrow(p)) return(NULL)
    p[, .(season = as.integer(season), team_id = as.integer(team_id), portal_share = num(portal_share),
          tal_in = num(transfer_talent_in), tal_out = num(transfer_talent_out), roster_n = num(roster_n))]
  }), fill = TRUE)
  # home stadium coordinates (latest file that exists), for travel distance
  info <- NULL
  for (s in rev(seasons)) {
    info <- read_release(url_team_info(s), 24 * 30)
    if (!is.null(info) && nrow(info)) break
  }
  coords <- if (!is.null(info)) unique(info[!is.na(latitude) & !is.na(longitude),
                     .(team_id = as.integer(team_id), venue_id = as.integer(venue_id),
                       lat = num(latitude), lon = num(longitude))], by = "team_id") else NULL
  list(talent = tal, returning = ret, portal = portal, coords = coords)
}

# ------------------------------------------------------- upcoming games
# ESPN's public scoreboard API (no key). Returns one row per game in the
# window, with ESPN's betting line when it carries one. Any failure returns
# NULL and the build carries on without an upcoming slate.
ESPN_SB <- "https://site.api.espn.com/apis/site/v2/sports/football/college-football/scoreboard?groups=80&limit=500"
fetch_espn_upcoming <- function(from, to) {
  espn_events(sprintf("%s&dates=%s-%s", ESPN_SB, format(from, "%Y%m%d"), format(to, "%Y%m%d")))
}
# The whole regular season, one week at a time (16 requests).
fetch_espn_season <- function(season) {
  out <- rbindlist(lapply(1:16, function(w) espn_events(sprintf("%s&seasontype=2&week=%d&dates=%d", ESPN_SB, w, season))), fill = TRUE)
  if (!nrow(out)) return(NULL)
  unique(out, by = "game_id")
}
espn_events <- function(url) {
  js <- tryCatch(suppressWarnings(jsonlite::fromJSON(url, simplifyVector = FALSE)), error = function(e) NULL)
  if (is.null(js) || !length(js$events)) return(NULL)
  rows <- lapply(js$events, function(ev) tryCatch({
    cp <- ev$competitions[[1]]
    tm <- cp$competitors
    hm <- tm[[which(vapply(tm, function(t) identical(t$homeAway, "home"), logical(1)))[1]]]
    aw <- tm[[which(vapply(tm, function(t) identical(t$homeAway, "away"), logical(1)))[1]]]
    od <- if (length(cp$odds)) cp$odds[[1]] else NULL
    spread <- NA_real_; total <- NA_real_
    if (!is.null(od)) {
      total <- suppressWarnings(as.numeric(od$overUnder %||% NA))
      sp <- suppressWarnings(as.numeric(od$spread %||% NA))
      home_fav <- od$homeTeamOdds$favorite %||% NA
      if (is.finite(sp) && !is.na(home_fav)) {
        spread <- if (isTRUE(home_fav)) -abs(sp) else abs(sp)
      } else if (!is.null(od$details) && is.character(od$details)) {
        # "OSU -7.5": favorite's abbreviation, then the line
        parts <- strsplit(trimws(od$details), " ")[[1]]
        v <- suppressWarnings(as.numeric(parts[length(parts)]))
        ab <- paste(parts[-length(parts)], collapse = " ")
        if (is.finite(v)) spread <- if (identical(ab, hm$team$abbreviation)) -abs(v) else abs(v)
      }
    }
    st <- cp$status$type %||% ev$status$type
    data.table(
      game_id = as.integer(ev$id), kickoff = as.character(ev$date %||% cp$date),
      week = as.integer(ev$week$number %||% NA), season_type = if (identical(as.integer(ev$season$type %||% 2L), 3L)) "postseason" else "regular",
      neutral = isTRUE(cp$neutralSite), conf_game = isTRUE(cp$conferenceCompetition),
      home_id = as.integer(hm$team$id), away_id = as.integer(aw$team$id),
      home_espn = as.character(hm$team$location %||% hm$team$displayName),
      away_espn = as.character(aw$team$location %||% aw$team$displayName),
      completed = isTRUE(st$completed), mkt_spread = spread, mkt_total = total,
      tbd = isTRUE(ev$timeValid == FALSE))
  }, error = function(e) NULL))
  out <- rbindlist(rows, fill = TRUE)
  if (!nrow(out)) return(NULL)
  out[, source := "ESPN"]
  out
}

# CollegeFootballData through cfbfastR, used only when ESPN fails and a
# CFBD_API_KEY is set, and only for seasons in CFBD_SEASONS. One call for
# the schedule plus one per upcoming week for lines.
fetch_cfbd_season <- function(season, from = as.Date("1900-01-01"), to = as.Date("2100-01-01")) {
  if (!cfbd_ok(season)) return(NULL)
  g <- cfbd_call("schedule", season, cfbfastR::cfbd_game_info(season, season_type = "regular"))
  if (is.null(g) || !nrow(g)) return(NULL)
  pick <- function(d, a) { a <- intersect(a, names(d)); if (length(a)) d[[a[1]]] else rep(NA, nrow(d)) }
  out <- data.table(
    game_id = as.integer(pick(g, c("game_id", "id"))),
    kickoff = as.character(pick(g, c("start_date", "startDate"))),
    week = as.integer(pick(g, "week")),
    season_type = as.character(pick(g, c("season_type", "seasonType"))),
    neutral = as.logical(pick(g, c("neutral_site", "neutralSite"))) %in% TRUE,
    conf_game = as.logical(pick(g, c("conference_game", "conferenceGame"))) %in% TRUE,
    home_id = as.integer(pick(g, c("home_id", "homeId"))), away_id = as.integer(pick(g, c("away_id", "awayId"))),
    home_espn = as.character(pick(g, c("home_team", "homeTeam"))), away_espn = as.character(pick(g, c("away_team", "awayTeam"))),
    completed = as.logical(pick(g, "completed")) %in% TRUE)
  out[, d := as.Date(substr(kickoff, 1, 10))]
  out <- out[d >= from & d <= to][, d := NULL]
  if (!nrow(out)) return(NULL)
  wk <- out[!completed & as.Date(substr(kickoff, 1, 10)) <= Sys.Date() + UPCOMING_DAYS, unique(week)]
  lines <- if (length(wk)) rbindlist(lapply(wk, function(w)
    cfbd_call(paste("betting lines, week", w), season, cfbfastR::cfbd_betting_lines(year = season, week = w))), fill = TRUE) else NULL
  out[, `:=`(mkt_spread = NA_real_, mkt_total = NA_real_)]
  if (!is.null(lines) && nrow(lines)) {
    lines <- lines[, .(game_id = as.integer(pick(lines, c("game_id", "id"))),
                       sp = suppressWarnings(as.numeric(pick(lines, "spread"))),
                       ou = suppressWarnings(as.numeric(pick(lines, c("over_under", "overUnder")))))]
    lines <- lines[, .(sp = median(sp, na.rm = TRUE), ou = median(ou, na.rm = TRUE)), by = game_id]
    out[lines, on = "game_id", `:=`(mkt_spread = i.sp, mkt_total = i.ou)]
  }
  out[, `:=`(tbd = FALSE, source = "CollegeFootballData")]
  out
}

# The public CollegeFootballData schedule release (no key, refreshed nightly
# upstream), used when neither ESPN nor the API answers. It carries every
# game of the season but no betting lines, so the slate shows no market.
fetch_release_season <- function(season) {
  cs <- read_release(url_cfbd_sched(season), REFRESH_HOURS)
  if (is.null(cs) || !nrow(cs)) return(NULL)
  cs <- cs[home_division %in% "fbs" | away_division %in% "fbs"]
  if (!nrow(cs)) return(NULL)
  out <- cs[, .(game_id = as.integer(game_id), kickoff = as.character(start_date), week = as.integer(week),
                season_type = fifelse(as.character(season_type) == "postseason", "postseason", "regular"),
                neutral = neutral_site %in% TRUE, conf_game = conference_game %in% TRUE,
                home_id = as.integer(home_id), away_id = as.integer(away_id),
                home_espn = as.character(home_team), away_espn = as.character(away_team),
                completed = completed %in% TRUE, mkt_spread = NA_real_, mkt_total = NA_real_,
                tbd = start_time_tbd %in% TRUE, venue_id = as.integer(venue_id),
                cfp = (!is.na(playoff_round) & nzchar(as.character(playoff_round))) | playoff_competition %in% TRUE)]
  out[, source := "the CollegeFootballData schedule release (no lines)"]
  out
}

# Team rosters with jersey numbers, used only to match defenders who are named in
# the play text but carry no id. ESPN's public roster endpoint first (one request
# per FBS team, cached for a day); CollegeFootballData's roster call (one call) if
# ESPN fails and a key is set. Returns team, id, jersey, first initial, last name,
# display name and position, or NULL.
fetch_rosters <- function(season, team_ids) {
  path <- file.path(CACHE_DIR, sprintf("rosters_%d.rds", season))
  if (file.exists(path) && difftime(Sys.time(), file.mtime(path), units = "hours") < 24) return(readRDS(path))
  one <- function(tid) {
    url <- sprintf("https://site.api.espn.com/apis/site/v2/sports/football/college-football/teams/%d/roster", tid)
    js <- tryCatch(suppressWarnings(jsonlite::fromJSON(url, simplifyVector = FALSE)), error = function(e) NULL)
    if (is.null(js) || !length(js$athletes)) return(NULL)
    # athletes come grouped by side ({position, items}) or as a flat list
    items <- if (!is.null(js$athletes[[1]]$items)) unlist(lapply(js$athletes, `[[`, "items"), recursive = FALSE) else js$athletes
    rbindlist(lapply(items, function(a) tryCatch(data.table(
      team = tid, id = suppressWarnings(as.integer(a$id)), jersey = suppressWarnings(as.integer(a$jersey %||% NA)),
      first = as.character(a$firstName %||% NA), last = as.character(a$lastName %||% NA),
      pos = as.character((a$position %||% list())$abbreviation %||% NA)), error = function(e) NULL)), fill = TRUE)
  }
  ros <- NULL
  first <- one(team_ids[1])
  if (!is.null(first) && nrow(first)) {
    ros <- rbindlist(c(list(first), lapply(team_ids[-1], one)), fill = TRUE)
    src <- "ESPN rosters"
  } else if (exists("cfbd_ok") && cfbd_ok(season)) {
    r <- cfbd_call("rosters (jersey match)", season, cfbfastR::cfbd_team_roster(year = season))
    if (!is.null(r) && nrow(r)) {
      r <- as.data.table(r)
      pick <- function(a) { a <- intersect(a, names(r)); if (length(a)) r[[a[1]]] else rep(NA, nrow(r)) }
      ros <- data.table(team = suppressWarnings(as.integer(pick(c("team_id", "teamId")))), id = suppressWarnings(as.integer(pick(c("athlete_id", "id")))),
                        jersey = suppressWarnings(as.integer(pick("jersey"))), first = as.character(pick(c("first_name", "firstName"))),
                        last = as.character(pick(c("last_name", "lastName"))), pos = as.character(pick("position")))
      src <- "CollegeFootballData rosters"
    }
  }
  if (is.null(ros) || !nrow(ros)) return(NULL)
  ros <- ros[!is.na(team) & !is.na(id)]
  ros[, `:=`(ini = substr(tc_norm(first), 1, 1), lastn = tc_norm(last), name = trimws(paste(first, last)))]
  attr(ros, "source") <- src
  saveRDS(ros, path)
  ros
}

# Team colors and abbreviations from ESPN's team list (no key). Optional.
fetch_espn_teams <- function() {
  url <- "https://site.api.espn.com/apis/site/v2/sports/football/college-football/teams?groups=80&limit=300"
  js <- tryCatch(suppressWarnings(jsonlite::fromJSON(url, simplifyVector = FALSE)), error = function(e) NULL)
  tl <- tryCatch(js$sports[[1]]$leagues[[1]]$teams, error = function(e) NULL)
  if (!length(tl)) return(NULL)
  rbindlist(lapply(tl, function(t) {
    t <- t$team
    data.table(id = as.integer(t$id), abbr = as.character(t$abbreviation %||% NA),
               color = as.character(t$color %||% NA), alt_color = as.character(t$alternateColor %||% NA),
               mascot = as.character(t$name %||% NA))
  }), fill = TRUE)
}

# =============================================================================
# Main build
# =============================================================================
run_build <- function() {

t_start <- Sys.time()
message("FBS dashboard build, season ", CFB_SEASON)
source(file.path(SCRIPT_DIR, "predict_cfb.R"))
source(file.path(SCRIPT_DIR, "players_cfb.R"))
source(file.path(SCRIPT_DIR, "simulate_cfb.R"))
source(file.path(SCRIPT_DIR, "recap_cfb.R"))
warnings_out <- if (exists("BUILD_NOTE", inherits = TRUE)) as.character(BUILD_NOTE) else character()

# ---- play-by-play, every season
message("Loading play-by-play")
sd <- list()
for (s in FIRST_SEASON:CFB_SEASON) {
  r <- tryCatch(prep_season(s), error = function(e) { message("  ", s, " failed: ", conditionMessage(e)); NULL })
  if (!is.null(r)) sd[[as.character(s)]] <- r
}
if (sum(as.integer(names(sd)) < CFB_SEASON) < 6)
  stop("Fewer than six past seasons of play-by-play loaded; check your connection and rerun.")
cur <- sd[[as.character(CFB_SEASON)]]
if (is.null(cur)) {
  # preseason: no release file for the new season yet
  warnings_out <- c(warnings_out, sprintf("No %d play-by-play yet, so everything shown is the preseason projection.", CFB_SEASON))
  cur <- list(games = sd[[1]]$games[0], tg = sd[[1]]$tg[0], tg_all = sd[[1]]$tg_all[0], players = sd[[1]]$players[0],
              games_fcs = NULL)
}
side <- load_side((FIRST_SEASON + 1):CFB_SEASON)
if (portal_missing(side, CFB_SEASON))
  warnings_out <- c(warnings_out, sprintf("No %d transfer-portal file yet: the preseason prior is refit without portal terms for this build.", CFB_SEASON))

# ---- players, and the roster terms they feed into the preseason prior
message("Player model")
PH <- build_player_history(sd, CACHE_DIR)
RF <- all_roster_features(sd, PH$players, (FIRST_SEASON + 2):CFB_SEASON, SCRIPT_DIR)
side$roster <- RF$features
side$roster_slot <- RF$by_slot
side$meta <- RF$meta                                  # c3.3: player projections per season, for the QB term
side$avail <- read_availability(SCRIPT_DIR)           # c3.3: cfb_availability.csv (team_id, athlete_id, action)
if (!is.null(side$avail)) message("  availability overrides: ", nrow(side$avail), " rows")
roster_src <- RF$meta[[as.character(CFB_SEASON)]]$source
if (!is.null(roster_src) && grepl("assumes everyone returns", roster_src))
  warnings_out <- c(warnings_out, "No preseason roster source, so the projection assumes every player from last season returns. Add cfb_roster_overrides.csv, or set a CFBD_API_KEY with this season in CFBD_SEASONS.")

# ---- calibration and backtest (cached after the first run of a season)
message("Model")
model <- run_cfb_model(sd, side, CFB_SEASON, CACHE_DIR)

# ---- FBS membership: this season's games, else last season's
fbs_ids <- unique(c(cur$games[home_div == "fbs", home_id], cur$games[away_div == "fbs", away_id]))
if (length(fbs_ids) < 100) {
  last <- sd[[as.character(CFB_SEASON - 1)]]$games
  fbs_ids <- unique(c(fbs_ids, last[home_div == "fbs", home_id], last[away_div == "fbs", away_id]))
}

# ---- schedule: the rest of the season (for the simulation) and the next few days (the slate)
message("Schedule")
today <- TODAY
season_sched <- fetch_espn_season(CFB_SEASON)
if (is.null(season_sched)) season_sched <- fetch_cfbd_season(CFB_SEASON)
if (is.null(season_sched)) season_sched <- fetch_release_season(CFB_SEASON)
if (is.null(season_sched)) season_sched <- fetch_espn_upcoming(today, today + UPCOMING_DAYS)
rest <- NULL; upcoming <- NULL
if (is.null(season_sched)) {
  warnings_out <- c(warnings_out, "Couldn't reach ESPN's scoreboard (or CollegeFootballData), so there's no upcoming slate or season simulation this build. The matchup tool still works.")
} else {
  rest <- season_sched[completed == FALSE & !(game_id %in% cur$games[completed == TRUE, game_id])]
  rest <- rest[home_id %in% fbs_ids | away_id %in% fbs_ids]
  rest[, `:=`(home_div = fifelse(home_id %in% fbs_ids, "fbs", "fcs"),
              away_div = fifelse(away_id %in% fbs_ids, "fbs", "fcs"))]
  known <- rbindlist(lapply(rev(sd), function(x) rbind(x$games[, .(id = home_id, name = home_name)],
                                                       x$games[, .(id = away_id, name = away_name)])))
  known <- unique(known, by = "id")
  rest[, home_name := known$name[match(home_id, known$id)]]
  rest[, away_name := known$name[match(away_id, known$id)]]
  rest[is.na(home_name), home_name := home_espn]
  rest[is.na(away_name), away_name := away_espn]
  rest[is.na(week), week := max(c(cur$games$week, 1L), na.rm = TRUE) + 1L]
  upcoming <- rest[as.Date(substr(kickoff, 1, 10)) <= today + UPCOMING_DAYS]
  message("  ", nrow(rest), " games left, ", nrow(upcoming), " in the next ", UPCOMING_DAYS, " days (", season_sched$source[1], ")")
}

live <- live_season(model, cur, side, CFB_SEASON, rest)
future_all <- live$upcoming            # every remaining game, for the Games tab's later weeks
if (!is.null(live$upcoming)) live$upcoming <- live$upcoming[game_id %in% upcoming$game_id]
rc <- report_card(model, live)

# ---- predictions log: save each upcoming pick before kickoff, grade later
log_cols <- c("game_id", "season", "week", "kickoff", "home_id", "away_id", "home_name", "away_name",
              "pred_margin", "p_home", "pred_total", "mkt_spread", "mkt_total", "logged_at")
plog <- if (file.exists(PRED_LOG_FILE)) fread(PRED_LOG_FILE, colClasses = list(character = c("kickoff", "logged_at"))) else
  setnames(data.table(matrix(nrow = 0, ncol = length(log_cols))), log_cols)
to_utc <- function(x) as.POSIXct(substr(sub(" ", "T", as.character(x)), 1, 16), format = "%Y-%m-%dT%H:%M", tz = "UTC")
# a pick logged after its kickoff isn't a pregame pick: flag it so it is never graded as "Saved"
if (nrow(plog)) plog[, late := (to_utc(logged_at) > to_utc(kickoff)) %in% TRUE] else plog[, late := logical()]
if (!is.null(live$upcoming) && nrow(live$upcoming)) {
  now_utc <- format(Sys.time(), "%Y-%m-%dT%H:%MZ", tz = "UTC")
  new <- live$upcoming[, .(game_id, season = CFB_SEASON, week, kickoff, home_id, away_id, home_name, away_name,
                           pred_margin = round(pm, 2), p_home = round(p_home, 4), pred_total = round(pt, 1),
                           mkt_spread, mkt_total, logged_at = now_utc)]
  # only games that haven't kicked off are logged or re-logged; started games keep what they had
  new <- new[(to_utc(kickoff) > Sys.time()) %in% TRUE]
  new[, late := FALSE]
  plog <- rbind(plog[!(game_id %in% new$game_id)], new, fill = TRUE)
  fwrite(plog, PRED_LOG_FILE)
}

# =============================================================================
# Dashboard data
# =============================================================================
message("Assembling dashboard data")
R <- live$ratings
tm <- live$teams
espn_t <- fetch_espn_teams()
games_c <- cur$games[completed == TRUE]

# conference: latest game's conference this season, else last season's
conf_now <- rbind(games_c[, .(id = home_id, conf = home_conf, d = game_date)], games_c[, .(id = away_id, conf = away_conf, d = game_date)])
setorder(conf_now, -d); conf_now <- unique(conf_now, by = "id")
last_g <- sd[[as.character(CFB_SEASON - 1)]]$games
conf_last <- rbind(last_g[, .(id = home_id, conf = home_conf, d = game_date)], last_g[, .(id = away_id, conf = away_conf, d = game_date)])
setorder(conf_last, -d); conf_last <- unique(conf_last, by = "id")

fbs <- R[div == "fbs" & id %in% fbs_ids]
fbs[, name := tm$name[match(id, tm$id)]]
fbs[, conf := conf_now$conf[match(id, conf_now$id)]]
fbs[is.na(conf), conf := conf_last$conf[match(id, conf_last$id)]]
fbs[is.na(conf), conf := "FBS Independents"]
if (!is.null(espn_t)) {
  fbs[, `:=`(abbr = espn_t$abbr[match(id, espn_t$id)], color = espn_t$color[match(id, espn_t$id)],
             alt_color = espn_t$alt_color[match(id, espn_t$id)])]
} else fbs[, `:=`(abbr = NA_character_, color = NA_character_, alt_color = NA_character_)]

# records
res <- rbind(games_c[, .(id = home_id, opp = away_id, pf = home_pts, pa = away_pts, conf_game, game_date)],
             games_c[, .(id = away_id, opp = home_id, pf = away_pts, pa = home_pts, conf_game, game_date)])
rec <- res[, .(w = sum(pf > pa), l = sum(pf < pa), cw = sum(pf > pa & conf_game), cl = sum(pf < pa & conf_game),
               pf = sum(pf), pa = sum(pa), gp = .N), by = id]
fbs <- merge(fbs, rec, by = "id", all.x = TRUE)
for (cc in c("w", "l", "cw", "cl", "pf", "pa", "gp")) fbs[is.na(get(cc)), (cc) := 0L]

# five factors, season to date, garbage time removed
tg <- cur$tg
sumtg <- function(by_col) tg[, .(plays = sum(plays), epa = sum(epa), succ = sum(succ), succ_epa = sum(succ_epa),
                                 expl = sum(expl), havoc = sum(havoc), to = sum(to), rush_n = sum(rush_n),
                                 rush_epa = sum(rush_epa), pass_n = sum(pass_n), pass_epa = sum(pass_epa),
                                 drives = sum(drives), start_ytg = sum(start_ytg_sum), opps = sum(opps),
                                 opp_pts = sum(opp_pts), drive_pts = sum(drive_pts), games = uniqueN(game_id)),
                             by = c(by_col)]
o <- sumtg("off_id"); setnames(o, "off_id", "id")
d <- sumtg("def_id"); setnames(d, "def_id", "id")
pace <- cur$tg_all[, .(plays_all = sum(plays), g = uniqueN(game_id)), by = .(id = off_id)]
ff <- merge(o[, .(id, epa_o = epa / plays, sr_o = succ / plays, iso_o = succ_epa / pmax(succ, 1), expl_o = expl / plays,
                  havoc_o = havoc / plays, rush_epa_o = rush_epa / pmax(rush_n, 1), pass_epa_o = pass_epa / pmax(pass_n, 1),
                  rush_rate = rush_n / plays, fp_o = 100 - start_ytg / pmax(drives, 1), ppo_o = opp_pts / pmax(opps, 1),
                  ppd_o = drive_pts / pmax(drives, 1), to_lost = to, games_o = games)],
            d[, .(id, epa_d = epa / plays, sr_d = succ / plays, iso_d = succ_epa / pmax(succ, 1), expl_d = expl / plays,
                  havoc_d = havoc / plays, rush_epa_d = rush_epa / pmax(rush_n, 1), pass_epa_d = pass_epa / pmax(pass_n, 1),
                  fp_d = 100 - start_ytg / pmax(drives, 1), ppo_d = opp_pts / pmax(opps, 1),
                  ppd_d = drive_pts / pmax(drives, 1), to_gained = to)], by = "id", all = TRUE)
ff <- merge(ff, pace[, .(id, pace = plays_all / g)], by = "id", all.x = TRUE)
ff[, to_margin_pg := (to_gained - to_lost) / pmax(games_o, 1)]
fbs <- merge(fbs, ff, by = "id", all.x = TRUE)

# traditional team stats, season to date, every play (box-score style). _o is
# the offense, _d what the defense allowed; per game unless the name says total.
trad_cols <- character()
bx <- cur$box
if (!is.null(bx) && nrow(bx)) {
  num_cols <- setdiff(names(bx), c("game_id", "off_id", "def_id"))
  agg_side <- function(by_col) {
    a <- bx[, c(lapply(.SD, sum), list(g = uniqueN(game_id))), by = c(by_col), .SDcols = num_cols]
    setnames(a, by_col, "id")
    a[, .(id, g, ypg = yds / g, ypp = yds / pmax(plays, 1), pass_ypg = pass_yds / g, ypa = pass_yds / pmax(pass_att, 1),
          cmp_pct = cmp / pmax(pass_att, 1), pass_att_g = pass_att / g, pass_td = pass_td, ints = ints,
          rush_ypg = rush_yds / g, ypc = rush_yds / pmax(rush_att, 1), rush_att_g = rush_att / g, rush_td = rush_td,
          fd_g = (fd + pen_fd) / g, d3_pct = d3c / pmax(d3, 1), d3_n = d3, d4_pct = d4c / pmax(d4, 1), d4_n = d4,
          rz_pct = rz_sc / pmax(rz, 1), rz_td_pct = rz_td / pmax(rz, 1), rz_n = rz,
          to_g = (ints + fum_lost) / g, to_n = ints + fum_lost, sacks_g = sacks / g,
          sack_rate = sacks / pmax(pass_att + sacks, 1), big_g = big / g,
          top_min = top / 60 / g, fgm = fgm, fga = fga, punts_g = punts / g, punt_avg = punt_yds / pmax(punts, 1))]
  }
  to <- agg_side("off_id"); td_ <- agg_side("def_id")
  keep_o <- setdiff(names(to), c("id", "g")); keep_d <- setdiff(keep_o, c("top_min", "fgm", "fga", "punts_g", "punt_avg"))
  setnames(to, keep_o, paste0("t_", keep_o, "_o"))
  td_ <- td_[, c("id", keep_d), with = FALSE]; setnames(td_, keep_d, paste0("t_", keep_d, "_d"))
  tt <- merge(to[, !"g"], td_, by = "id", all = TRUE)
  trad_cols <- setdiff(names(tt), "id")
  fbs <- merge(fbs, tt, by = "id", all.x = TRUE)
}

# luck (wins above pregame expectation) and schedule strength
pl <- live$played
if (!is.null(pl) && nrow(pl)) {
  pg <- rbind(pl[completed == TRUE, .(id = home_id, opp = away_id, pwin = p_home, won = home_pts > away_pts)],
              pl[completed == TRUE, .(id = away_id, opp = home_id, pwin = 1 - p_home, won = away_pts > home_pts)])
  pg[, opp_net := R$net[match(opp, R$id)]]
  lk <- pg[, .(exp_w = sum(pwin), luck = sum(won) - sum(pwin), sos = mean(opp_net, na.rm = TRUE)), by = id]
  fbs <- merge(fbs, lk, by = "id", all.x = TRUE)
} else fbs[, `:=`(exp_w = NA_real_, luck = NA_real_, sos = NA_real_)]

setorder(fbs, -net)
fbs[, rank := seq_len(.N)]
fbs[, pre_rank := frank(-pre_net, ties.method = "min")]

r2 <- function(x, n = 2) ifelse(is.finite(x), round(x, n), NA)
teams_out <- fbs[, .(id, name, abbr, color, alt_color, conf, rank, pre_rank, w, l, cw, cl, gp, pf, pa,
                     net = r2(net, 1), off = r2(off, 1), def = r2(def, 1), pre_net = r2(pre_net, 1),
                     adj_epa_o = r2(adj_epa_o, 3), adj_epa_d = r2(adj_epa_d, 3),
                     adj_sr_o = r2(adj_sr_o, 4), adj_sr_d = r2(adj_sr_d, 4),
                     epa_o = r2(epa_o, 3), epa_d = r2(epa_d, 3), sr_o = r2(sr_o, 4), sr_d = r2(sr_d, 4),
                     iso_o = r2(iso_o, 3), iso_d = r2(iso_d, 3), expl_o = r2(expl_o, 4), expl_d = r2(expl_d, 4),
                     rush_epa_o = r2(rush_epa_o, 3), rush_epa_d = r2(rush_epa_d, 3),
                     pass_epa_o = r2(pass_epa_o, 3), pass_epa_d = r2(pass_epa_d, 3),
                     fp_o = r2(fp_o, 1), fp_d = r2(fp_d, 1), ppo_o = r2(ppo_o, 2), ppo_d = r2(ppo_d, 2),
                     ppd_o = r2(ppd_o, 2), ppd_d = r2(ppd_d, 2), havoc_d = r2(havoc_d, 4), havoc_o = r2(havoc_o, 4),
                     to_margin_pg = r2(to_margin_pg, 2), pace = r2(pace, 1), rush_rate = r2(rush_rate, 3),
                     exp_w = r2(exp_w, 2), luck = r2(luck, 2), sos = r2(sos, 1))]
if (length(trad_cols)) {
  tt_out <- fbs[, c("id", trad_cols), with = FALSE]
  for (cc in trad_cols) set(tt_out, j = cc, value = r2(tt_out[[cc]], 4))
  teams_out <- merge(teams_out, tt_out, by = "id", all.x = TRUE, sort = FALSE)
  setorder(teams_out, rank)
}

# ---- per-team game logs (this season), with the model's pregame numbers
nm <- function(ids) { x <- tm$name[match(ids, tm$id)]; ifelse(is.na(x), "", x) }
logs <- NULL
if (!is.null(pl) && nrow(pl)) {
  side_rows <- function(home) {
    if (home) pl[, .(game_id, id = home_id, opp = away_id, loc = fifelse(neutral, "N", "H"), date = as.character(game_date), week,
                     season_type, pf = home_pts, pa = away_pts, pm = pm, p = p_home, spread = spread, conf_game)]
    else pl[, .(game_id, id = away_id, opp = home_id, loc = fifelse(neutral, "N", "A"), date = as.character(game_date), week,
                season_type, pf = away_pts, pa = home_pts, pm = -pm, p = 1 - p_home, spread = -spread, conf_game)]
  }
  logs <- rbind(side_rows(TRUE), side_rows(FALSE))
  # "spread" is now each team's own line (negative = favored); the pbp's
  # line is the home team's, from CollegeFootballData
  gstat <- tg[, .(game_id, id = off_id, g_epa_o = epa / plays, g_sr_o = succ / plays)]
  gstat_d <- tg[, .(game_id, id = def_id, g_epa_d = epa / plays, g_sr_d = succ / plays)]
  logs <- merge(logs, gstat, by = c("game_id", "id"), all.x = TRUE)
  logs <- merge(logs, gstat_d, by = c("game_id", "id"), all.x = TRUE)
  logs <- logs[id %in% fbs$id]
  logs[, opp_name := nm(opp)]
  logs[, opp_rank := fbs$rank[match(opp, fbs$id)]]
  logs[, opp_fcs := !(opp %in% fbs_ids)]
  logs[, `:=`(pm = r2(pm, 1), p = r2(p, 3), g_epa_o = r2(g_epa_o, 3), g_epa_d = r2(g_epa_d, 3),
              g_sr_o = r2(g_sr_o, 3), g_sr_d = r2(g_sr_d, 3))]
  setorder(logs, id, date)
}

# ---- leaders (raw, garbage time removed)
pls <- cur$players[, .(n = sum(n), epa = sum(epa), succ = sum(succ)), by = .(team, player, role)]
pls <- pls[team %in% fbs$id]
team_pass <- pls[role == "pass", .(tp = sum(n)), by = team]
pls <- merge(pls, team_pass, by = "team", all.x = TRUE)
setorder(pls, team, role, -n)
leaders <- pls[, head(.SD, if (role[1] == "pass") 2 else 4), by = .(team, role)][
  , .(team, role, player, n, epa_pp = r2(epa / n, 3), sr = r2(succ / n, 3))]

# ---- upcoming slate
slate <- NULL
if (!is.null(live$upcoming) && nrow(live$upcoming)) {
  u <- live$upcoming
  slate <- u[, .(game_id, kickoff, week, season_type, neutral, conf_game, home_id, away_id, home_name, away_name, home_div, away_div,
                 pm = r2(pm, 1), pt = r2(pt, 1), p_home = r2(p_home, 3), mkt_spread, mkt_total, tbd)]
}

# ---- the rest of the schedule past the slate, with the model's numbers (no market lines)
schedule_out <- NULL
if (!is.null(future_all) && nrow(future_all)) {
  schedule_out <- future_all[!(game_id %in% slate$game_id),
    .(game_id, kickoff, week, season_type, neutral, conf_game, home_id, away_id, home_name, away_name,
      pm = r2(pm, 1), pt = r2(pt, 1), p_home = r2(p_home, 3), tbd)]
  setorder(schedule_out, kickoff)
}

# ---- graded picks this season: saved picks first, replayed pregame numbers otherwise
graded <- NULL
if (!is.null(pl) && nrow(pl)) {
  gd <- pl[completed == TRUE, .(game_id, week, season_type, cfp, date = as.character(game_date), home_id, away_id, home_pts, away_pts,
                                pm, p_home, pt, spread, over_under, neutral, conf_game)]
  gd[, saved := FALSE]
  if (nrow(plog)) {
    lg <- plog[season == CFB_SEASON & !(late %in% TRUE), .(game_id, l_pm = pred_margin, l_p = p_home, l_pt = pred_total, l_sp = mkt_spread)]
    gd <- merge(gd, lg, by = "game_id", all.x = TRUE)
    gd[!is.na(l_pm), `:=`(pm = l_pm, p_home = l_p, pt = l_pt, saved = TRUE)]
    gd[!is.na(l_sp) & is.na(spread), spread := l_sp]
    gd[, c("l_pm", "l_p", "l_pt", "l_sp") := NULL]
  }
  gd[, `:=`(home_name = nm(home_id), away_name = nm(away_id), pm = r2(pm, 1), p_home = r2(p_home, 3), pt = r2(pt, 1))]
  graded <- gd[home_id %in% fbs$id | away_id %in% fbs$id]
  graded_cfp <- graded$cfp; graded[, cfp := NULL]
}

# ---- conferences
confs <- fbs[, .(teams = .N, avg_net = r2(mean(net), 1), best = name[which.max(net)], best_id = id[which.max(net)],
                 top25 = sum(rank <= 25), avg_rank = r2(mean(rank), 1)), by = conf][order(-avg_net)]

# ---- season simulation
sim_out <- NULL; sim_meta <- NULL
if (!is.null(rest) && nrow(rest[season_type == "regular"])) {
  message("Simulating the season (", SIM_N, " runs)")
  conf_of <- setNames(fbs$conf, fbs$id)
  s0 <- prior_miss_sd(model$cal, live$layer)
  sim <- tryCatch(simulate_season(live, cur$games, rest[season_type == "regular"], conf_of, fbs$id, s0),
                  error = function(e) {
                    message("  simulation failed: ", conditionMessage(e))
                    warnings_out <<- c(warnings_out, paste("The season simulation failed:", conditionMessage(e)))
                    NULL })
  if (!is.null(sim)) {
    sim_out <- sim[, .(id, w = r2(w_mean, 2), l = r2(l_mean, 2), cw = r2(cw_mean, 2), cl = r2(cl_mean, 2),
                       p_bowl = r2(p_bowl, 4), p_ccg = r2(p_ccg, 4), p_conf = r2(p_conf, 4), p_cfp = r2(p_cfp, 4),
                       p_bye = r2(p_bye, 4), p_qf = r2(p_qf, 4), p_sf = r2(p_sf, 4), p_final = r2(p_final, 4),
                       p_title = r2(p_title, 4), seed = r2(seed_mean, 2))]
    sim_meta <- list(n = SIM_N, games_left = nrow(rest[season_type == "regular"]), s0 = r2(s0, 2),
                     sig_game = r2(attr(sim, "sig_game"), 2), win_pts = SIM_WIN_PTS)
  }
} else if (!is.null(rest)) {
  warnings_out <- c(warnings_out, "No regular-season games left on the schedule, so the season simulation is off.")
}

# ---- players
message("Player tables")
ph <- PH$players
cur_p <- ph[season == CFB_SEASON & team %in% fbs$id]
cur_p <- cur_p[(role == "qb" & n_cred >= PL_MIN_DISPLAY[["qb"]]) | (role == "rush" & n >= PL_MIN_DISPLAY[["rush"]]) |
               (role == "rec" & n_cred >= PL_MIN_DISPLAY[["rec"]])]
prev <- ph[season == CFB_SEASON - 1L, .(id, role, prev_eff = eff, prev_n = round(n_cred))]
cur_p <- merge(cur_p, prev, by = c("id", "role"), all.x = TRUE)
pj <- RF$meta[[as.character(CFB_SEASON)]]$proj
if (!is.null(pj)) cur_p <- merge(cur_p, pj[, .(id, role, proj_eff = proj)], by = c("id", "role"), all.x = TRUE) else cur_p[, proj_eff := NA_real_]
# team pass attempts, for target share
tpa <- cur$plays[is_pass == TRUE & sack == FALSE, .(tpa = .N), by = .(team = off_id)]
cur_p <- merge(cur_p, tpa, by = "team", all.x = TRUE)
cur_p[, share := fifelse(role == "rec", n_cred / tpa, NA_real_)]
players_out <- cur_p[, .(id, name, team, pos, role, n = round(n_cred), games, eff = r2(eff, 3), value_pg = r2(value_pg, 2),
                         raw_epa = r2(raw_epa, 3), sr = r2(sr, 3), share = r2(share, 3),
                         prev_eff = r2(prev_eff, 3), proj_eff = r2(proj_eff, 3))]
career <- ph[id %in% players_out$id, .(id, season, team, role, n = round(n_cred), eff = r2(eff, 3), value_pg = r2(value_pg, 2),
                                       raw_epa = r2(raw_epa, 3))][order(id, season, role)]
career[, team_name := nm(team)]
dfp <- cur$defp
defense_out <- NULL
if (!is.null(dfp) && nrow(dfp)) {
  defense_out <- dcast(dfp[team %in% fbs$id], id + team ~ what, value.var = "N", fun.aggregate = sum)
  nmpos <- dfp[, .(name = mode1(name), pos = mode1(pos)), by = id]
  defense_out <- merge(defense_out, nmpos, by = "id")
  for (cc in c("sack", "int", "pbu", "ff")) if (!cc %in% names(defense_out)) defense_out[, (cc) := 0L]
  defense_out[, plays := sack + int + pbu + ff]
  defense_out <- defense_out[plays >= 2][order(-plays)]
}
# ---- traditional player stats (box-score style, every play) and game logs
trad <- NULL
pbx <- cur$pbox
text_match <- NULL
if (!is.null(pbx) && !is.null(pbx$text) && nrow(pbx$text)) {
  # breakups and forced fumbles named only in the text: match to a player by
  # team, jersey and last name, then by team, first initial and last name (only
  # when exactly one player fits). The pool is the roster (if one could be
  # fetched) plus every defender credited with an id this season and the two before.
  ros <- tryCatch(fetch_rosters(CFB_SEASON, fbs$id), error = function(e) NULL)
  pool <- rbindlist(lapply(as.character(CFB_SEASON - 0:2), function(s) sd[[s]]$pbox$seen), fill = TRUE)
  rcols <- c("team", "id", "jersey", "ini", "lastn")
  pool <- unique(rbind(if (!is.null(ros)) ros[, ..rcols], if (nrow(pool)) pool[, ..rcols], fill = TRUE))
  tx <- copy(pbx$text)[, key := .I]
  one_id <- function(id) if (uniqueN(id) == 1) id[1] else NA_integer_
  m1 <- merge(tx[!is.na(jersey), .(key, team, jersey, lastn)], unique(pool[!is.na(jersey), .(team, jersey, lastn, id)]),
              by = c("team", "jersey", "lastn"))[, .(id1 = one_id(id)), by = key]
  m2 <- merge(tx[, .(key, team, ini, lastn)], unique(pool[, .(team, ini, lastn, id)]),
              by = c("team", "ini", "lastn"))[, .(id2 = one_id(id)), by = key]
  tx <- merge(merge(tx, m1, by = "key", all.x = TRUE), m2, by = "key", all.x = TRUE)
  tx[, id := fcoalesce(id1, id2)]
  # matched credits join the box lines; names for roster-only players come from the roster
  long <- if (!is.null(pbx$def) && nrow(pbx$def)) melt(pbx$def, id.vars = c("game_id", "team", "id"), variable.name = "what", value.name = "N",
                                                      variable.factor = FALSE) else NULL
  long <- rbind(long, tx[!is.na(id), .N, by = .(game_id, team, id, what)], fill = TRUE)
  pbx$def <- dcast(long[, .(N = sum(N)), by = .(game_id, team, id, what)], game_id + team + id ~ what, value.var = "N", fill = 0L)
  if (!is.null(ros)) {
    add <- unique(ros[!(id %in% pbx$names$id), .(id, name, pos)], by = "id")
    pbx$names <- rbind(pbx$names, add, fill = TRUE)
  }
  # the rest stay listed under the name in the text ("S. Soles Jr."), with no page
  text_match <- list(n = nrow(tx), matched = sum(!is.na(tx$id)), roster = if (!is.null(ros)) attr(ros, "source") else NA,
                     unmatched = tx[is.na(id), .(g = uniqueN(game_id), N = .N), by = .(team, label, what)])
  message("  defensive credits named only in the text: ", text_match$matched, " of ", text_match$n, " matched to a player",
          if (!is.null(ros)) paste0(" (with ", attr(ros, "source"), ")") else " (no roster source reached)")
}
if (!is.null(pbx) && !is.null(pbx$pass)) {
  pn <- pbx$names
  name_of <- function(ids) pn$name[match(ids, pn$id)]; pos_of <- function(ids) pn$pos[match(ids, pn$id)]
  main_team <- function(d) d[, .(team = as.integer(names(which.max(table(team))))), by = id]
  season_tab <- function(d, cols, extra = NULL) {
    if (is.null(d) || !nrow(d)) return(NULL)
    d <- d[team %in% fbs$id]
    a <- d[, c(lapply(.SD, sum), list(g = uniqueN(game_id))), by = id, .SDcols = setdiff(cols, "lng")]
    if ("lng" %in% cols) a <- merge(a, d[, .(lng = max(lng)), by = id], by = "id")
    a <- merge(a, main_team(d), by = "id")
    a[, `:=`(name = name_of(id), pos = pos_of(id))]
    a[!is.na(name)]
  }
  tp <- season_tab(pbx$pass, c("att", "cmp", "yds", "td", "int", "sk", "lng"))
  if (!is.null(tp)) {
    tp <- tp[att >= 5]
    tp[, `:=`(cmp_pct = r2(cmp / att, 3), ypa = r2(yds / att, 1), ypg = r2(yds / g, 1),
              rating = r2((8.4 * yds + 330 * td + 100 * cmp - 200 * int) / att, 1))]
  }
  tr <- season_tab(pbx$rush, c("att", "yds", "td", "lng"))
  if (!is.null(tr)) { tr <- tr[att >= 3]; tr[, `:=`(ypc = r2(yds / att, 1), ypg = r2(yds / g, 1))] }
  tc <- season_tab(pbx$rec, c("rec", "yds", "td", "lng"))
  if (!is.null(tc)) { tc <- tc[rec >= 2]; tc[, `:=`(ypr = r2(yds / rec, 1), ypg = r2(yds / g, 1))] }
  tdf <- NULL
  if (!is.null(pbx$def) && nrow(pbx$def)) {
    dd <- copy(pbx$def)
    for (cc in c("sack", "int", "pbu", "ff", "fr")) if (!cc %in% names(dd)) dd[, (cc) := 0L]
    tdf <- season_tab(dd, c("sack", "int", "pbu", "ff", "fr"))
    if (!is.null(tdf)) {
      tdf[, total := sack + int + pbu + ff + fr]
      # some seasons' play-by-play credits no one at all with breakups or forced
      # fumbles (2026 so far); show those as missing, not as zeros
      if (!is.null(text_match) && nrow(text_match$unmatched)) {
        um <- dcast(text_match$unmatched[team %in% fbs$id], team + label ~ what, value.var = "N", fun.aggregate = sum, fill = 0L)
        gg <- text_match$unmatched[team %in% fbs$id, .(g = max(g)), by = .(team, label)]
        um <- merge(um, gg, by = c("team", "label"))
        for (cc in c("sack", "int", "pbu", "ff", "fr")) if (!cc %in% names(um)) um[, (cc) := 0L]
        um[, `:=`(id = NA_integer_, name = sub("^([A-Za-z'\\-]+)\\.", "\\1. ", label), pos = NA_character_, total = sack + int + pbu + ff + fr)]
        tdf <- rbind(tdf, um[, names(tdf), with = FALSE])
      }
      for (cc in c("pbu", "ff", "fr")) if (sum(tdf[[cc]]) == 0) set(tdf, j = cc, value = NA_integer_)
    }
  }
  tk <- NULL
  if (!is.null(pbx$kick) && nrow(pbx$kick)) {
    kd <- pbx$kick[team %in% fbs$id]
    tk <- kd[, .(id = id[!is.na(id)][1], g = uniqueN(game_id), fga = sum(fga), fgm = sum(fgm), lng = max(lng),
                 fg40 = sum(fg40), fgm40 = sum(fgm40)), by = .(team, kname = name)]
    tk[, `:=`(name = fcoalesce(name_of(id), kname), pos = "PK", fg_pct = r2(fgm / pmax(fga, 1), 3))]
    tk[, kname := NULL]
  }
  trad <- list(pass = tp, rush = tr, rec = tc, def = tdf, kick = tk)
}

# ---- game recaps (c3.4): every completed game's line score, win probability,
# scoring plays, drives, key plays, team box and every player's line with EPA
message("Game recaps")
vals <- function(d) jsonlite::toJSON(d, dataframe = "values", na = "null", digits = NA)
colrows <- function(d) if (is.null(d) || !nrow(d)) NULL else list(cols = names(d), rows = vals(d))
rp <- if (nrow(games_c)) tryCatch(prep_recap_plays(CFB_SEASON, cur$games), error = function(e) {
  message("  recaps failed: ", conditionMessage(e))
  warnings_out <<- c(warnings_out, paste("Game recaps couldn't be built:", conditionMessage(e)))
  NULL }) else NULL
recaps_out <- NULL; recap_meta <- NULL
if (!is.null(rp) && !is.null(graded)) {
  pg_model <- tryCatch(fit_pgwe(sd, FIRST_SEASON:(CFB_SEASON - 1L)), error = function(e) NULL)
  pg <- apply_pgwe(pg_model, pgwe_frame(cur))
  gwp <- copy(graded)[, cfp := graded_cfp]
  gwp[, phase := phase_of(week, season_type, cfp)]
  cs_now <- read_release(url_cfbd_sched(CFB_SEASON), REFRESH_HOURS)
  rc_b <- tryCatch(build_recaps(rp, gwp, live$layer$sigma, pg, cs_now), error = function(e) {
    message("  recaps failed: ", conditionMessage(e))
    warnings_out <<- c(warnings_out, paste("Game recaps couldn't be built:", conditionMessage(e)))
    NULL })
  if (!is.null(rc_b)) {
    recaps_out <- lapply(rc_b$recaps, function(o) {
      for (k in c("sc", "kp", "dr")) if (!is.null(o[[k]])) o[[k]] <- vals(o[[k]])
      o })
    recap_meta <- list(dr = rc_b$dr_results, wp_brier = rc_b$wp_check$brier, wp_brier_pbp = rc_b$wp_check$brier_pbp, wp_n = rc_b$wp_check$n,
                       pgwe = if (!is.null(pg_model)) list(r2 = r2(pg_model$r2, 3), sigma = r2(pg_model$sig_full, 2), n = pg_model$n,
                                                          from = pg_model$seasons[1], to = pg_model$seasons[2]) else NULL)
    message("  ", length(recaps_out), " recaps; win probability Brier ", rc_b$wp_check$brier,
            " (the play-by-play's own column on the same plays: ", rc_b$wp_check$brier_pbp, ")",
            if (!is.null(pg_model)) sprintf("; postgame win expectancy R2 %.3f on %d games", pg_model$r2, pg_model$n) else "")
  }
}
# team box per game: box score (every play) and efficiency (garbage time removed)
gteam <- NULL
if (!is.null(bx) && nrow(bx)) {
  g1 <- bx[, .(game_id, team = off_id, plays, yds, pass_att, cmp, pass_yds, pass_td, ints, sacks, sack_yds, rush_att, rush_yds,
               rush_td, fum_lost, fd = fd + pen_fd, big, d3, d3c, d4, d4c, rz, rz_td, rz_sc, top = round(top), fga, fgm, punts, punt_yds)]
  g2 <- tg[, .(game_id, team = off_id, c_pl = plays, c_epa = round(epa, 2), c_succ = succ, r_n = rush_n, r_epa = round(rush_epa, 2),
               p_n = pass_n, p_epa = round(pass_epa, 2), expl, havoc, drives, fp = start_ytg_sum, opps, opp_pts, dpts = drive_pts)]
  gteam <- colrows(merge(g1, g2, by = c("game_id", "team"), all.x = TRUE))
}
# every player's line in every completed game, with EPA on his plays (replaces
# c3.1's tlog, which kept only regulars): the recap box scores and the player pages' game logs
gbox <- NULL; pnames <- NULL
if (!is.null(pbx) && !is.null(pbx$pass)) {
  pe <- if (!is.null(rp)) rp$pe else NULL
  with_epa <- function(d, role_, cols) {
    if (is.null(d) || !nrow(d)) return(NULL)
    d <- copy(d)
    if (!is.null(pe)) {
      e <- pe[role == role_, .(game_id, team, id, epa = round(epa, 2), sr = round(succ / n, 3))]
      d <- merge(d, e, by = c("game_id", "team", "id"), all.x = TRUE)
    } else d[, `:=`(epa = NA_real_, sr = NA_real_)]
    colrows(d[, c("id", "game_id", "team", cols, "epa", "sr"), with = FALSE])
  }
  ddf <- if (!is.null(pbx$def) && nrow(pbx$def)) { x <- copy(pbx$def); for (cc in c("sack", "int", "pbu", "ff", "fr")) if (!cc %in% names(x)) x[, (cc) := 0L]; x } else NULL
  gbox <- list(pass = with_epa(pbx$pass, "pass", c("att", "cmp", "yds", "td", "int", "sk", "lng")),
               rush = with_epa(pbx$rush, "rush", c("att", "yds", "td", "lng")),
               rec = with_epa(pbx$rec, "rec", c("tgt", "rec", "yds", "td", "lng")),
               def = if (!is.null(ddf)) colrows(ddf[!is.na(id), .(id, game_id, team, sack, int, pbu, ff, fr)]) else NULL,
               kick = if (!is.null(pbx$kick) && nrow(pbx$kick)) colrows(pbx$kick[, .(id, game_id, team, name, fga, fgm, lng)]) else NULL)
  ids <- unique(c(pbx$pass$id, pbx$rush$id, pbx$rec$id, if (!is.null(ddf)) ddf$id))
  pn <- unique(pbx$names[id %in% ids & !is.na(name)], by = "id")
  pnames <- setNames(lapply(seq_len(nrow(pn)), function(i) list(pn$name[i], pn$pos[i])), pn$id)
}

# team-level: run game (blocking and scheme) and the preseason roster terms
rb <- PH$team_run[season == CFB_SEASON, .(id = team, run_block = r2(run_block, 3))]
rfc <- RF$features[season == CFB_SEASON, .(id, qb_proj = r2(qb_proj, 3), qb_delta = r2(qb_delta, 3),
                                          skill_proj = r2(skill_proj, 2), skill_delta = r2(skill_delta, 2), qb_new)]
qbsel <- RF$meta[[as.character(CFB_SEASON)]]$qb
if (!is.null(qbsel) && nrow(qbsel)) {
  qn <- ph[role == "qb", .(name = name[which.max(season)]), by = id]
  rfc <- merge(rfc, qbsel[, .(id = team, qb_id = id)], by = "id", all.x = TRUE)
  rfc[, qb_name := qn$name[match(qb_id, qn$id)]]
}
teams_out <- merge(teams_out, rb, by = "id", all.x = TRUE)
teams_out <- merge(teams_out, rfc, by = "id", all.x = TRUE)
setorder(teams_out, rank)
pmod <- RF$meta[[as.character(CFB_SEASON)]]$pm
player_meta <- list(
  text_credits = if (!is.null(text_match)) list(n = text_match$n, matched = text_match$matched, roster = text_match$roster) else NULL,
  lambda = as.list(PL_LAMBDA), k_rec = PL_K_REC, min = as.list(PL_MIN_DISPLAY), roster_source = roster_src,
  yoy = if (!is.null(pmod)) lapply(pmod, function(m) list(r = r2(m$r, 3), n = m$n, newcomer = r2(m$newcomer, 3))) else NULL)

# ---- matchup tool: every team that might be picked (FBS + FCS seen this or last season)
mt <- live$matchup
mt_teams <- data.table(id = mt$team_ids, name = nm(mt$team_ids), fbs = mt$team_ids %in% fbs_ids)
r4 <- function(x) round(as.numeric(x), 5)
# net / net_fcs: each team's rating in points for an FBS game and for a game
# with an FCS team, weighted for this point in the season (the page adds home
# field, travel and the gap); E, P and N build the total
matchup <- list(
  teams = mt_teams,
  net = r4(mt$net), net_fcs = r4(mt$net_fcs),
  P = list(mu = r4(mt$P$mu), O = r4(mt$P$O), D = r4(mt$P$D)),
  E = list(mu = r4(mt$E$mu), O = r4(mt$E$O), D = r4(mt$E$D)),
  N = list(mu = r4(mt$N$mu), O = r4(mt$N$O), D = r4(mt$N$D)),
  lat = r4(mt$lat), lon = r4(mt$lon),
  qb_dq = r4(mt$qb_dq),
  margin = lapply(mt$margin, r4), total = lapply(mt$total, r4), sigma = lapply(mt$sigma, r4))

last_date <- if (nrow(games_c)) as.character(max(games_c$game_date)) else NA
meta <- list(
  season = CFB_SEASON, built = format(Sys.time(), "%Y-%m-%d %H:%M %Z"),
  data_through = last_date, games_played = nrow(games_c),
  week = if (nrow(games_c)) max(games_c$week) else 0L,
  upcoming_source = if (!is.null(upcoming)) upcoming$source[1] else NA,
  warnings = as.list(warnings_out), version = PC_VERSION,
  avg_pts = r2(live$avg_pts, 1), hfa_pts = r2(live$layer$margin[["loc"]], 2))

data_blob <- list(meta = meta, teams = teams_out, logs = logs, leaders = leaders, slate = slate,
                  graded = graded, conferences = confs, matchup = matchup,
                  sim = sim_out, sim_meta = sim_meta, players = players_out, career = career,
                  defense = defense_out, player_meta = player_meta, trad = trad,
                  schedule = schedule_out, recaps = recaps_out, recap_meta = recap_meta,
                  gbox = gbox, gteam = gteam, pnames = pnames,
                  report = list(by_season = rc$by_season, by_phase = rc$by_phase, overall = rc$overall,
                                fbs_only = rc$fbs_only, calib = rc$calib, current = rc$current,
                                folds = rc$folds, live = rc$live,
                                prior_coef = lapply(model$cal$pmodels[[as.character(max(as.integer(names(model$cal$pmodels))))]],
                                                    function(m) as.list(round(m$coef, 4)))))

json <- jsonlite::toJSON(data_blob, dataframe = "rows", na = "null", auto_unbox = TRUE, digits = NA, json_verbatim = TRUE)
json <- gsub("</", "<\\/", as.character(json), fixed = TRUE)
tpl <- paste(readLines(TEMPLATE_FILE, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
if (!grepl("__CFB_DATA__", tpl, fixed = TRUE)) stop("cfb_template.html has no __CFB_DATA__ placeholder")
parts <- strsplit(tpl, "__CFB_DATA__", fixed = TRUE)[[1]]
html <- paste0(parts[1], json, paste(parts[-1], collapse = "__CFB_DATA__"))
writeLines(html, OUT_FILE, useBytes = TRUE)
message("CollegeFootballData API calls this build: ", cfbd_calls)
message(sprintf("Wrote %s (%.1f MB) in %.1f min", basename(OUT_FILE), file.size(OUT_FILE) / 1e6,
                as.numeric(difftime(Sys.time(), t_start, units = "mins"))))
invisible(data_blob)
}

# Runs on Rscript build_cfb.R or source("build_cfb.R").
# Set options(cfb.skip_build = TRUE) first to load the functions only.
if (!isTRUE(getOption("cfb.skip_build"))) run_build()
