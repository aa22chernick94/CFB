# ASPN FBS Football dashboard (R pipeline, c3.4)

Builds `cfb_dashboard.html`: opponent-adjusted power ratings for every FBS team, player ratings and traditional box-score stats, team and player pages, a matchup screen for every upcoming game, a weekly slate with model lines next to the market, a matchup predictor, a full-season simulation through the 12-team playoff, and a report card that grades the model against the closing line. It works the same way as the WBB build: one R script, one HTML template, one self-contained output file.

## Running it

Keep these in one folder and run `Rscript build_cfb.R` (or `source("build_cfb.R")` in RStudio).

| File | What it is |
|---|---|
| `build_cfb.R` | Downloads and caches the data, runs the model, writes the dashboard. |
| `predict_cfb.R` | Team model: ratings, preseason prior, backtest, predictions. Sourced by the build. |
| `players_cfb.R` | Player model, player projections and the roster terms in the preseason prior. |
| `simulate_cfb.R` | Season simulation: remaining games, conference title games, playoff. |
| `cfb_roster_overrides.csv` | *Optional.* Preseason roster fixes: `team_id, athlete_id, action` with action `add`, `exclude` or `starting_qb`. ESPN ids. |
| `recap_cfb.R` | Game recaps (c3.4): score and win probability through each game, scoring plays, drives, key plays, per-player EPA. Sourced by the build. |
| `cfb_template.html` | The dashboard page. The build drops the data in at `__CFB_DATA__`. |
| `cfb_dashboard.html` | A c3.4 sample built on 4 Oct 2026, through week 5. ESPN couldn't be reached where it was built, so the schedule comes from the CollegeFootballData release; team colors, abbreviations and the market lines on the next slate were copied in from the c3.3 build that ESPN had fed. Your own build replaces all of it. |
| `cfb_demo_2025_week6.html` | The 2025 season replayed as of week 6, with the schedule, so every tab is filled in. **Built with c2.1**, so its numbers are the old model's. |
| `predictions_log_cfb.csv` | *Written by the build.* Every upcoming pick, saved before kickoff and graded after. |
| `.cfb_cache/` | *Written by the build.* Downloaded files, processed seasons, the calibration. |

Packages: `data.table`, `jsonlite`, `nanoparquet`, `Matrix` (installed automatically if missing). `cfbfastR` is optional; see "Upcoming games".

**First run** downloads about 1 GB of play-by-play (2014 on), processes it (about 4 minutes), fits the player model (about 20 seconds) and calibrates the team model (about 3.5 minutes on one core). After that a build takes well under a minute, including 10,000 season simulations. The current season is re-downloaded when the cached copy is more than 6 hours old (`REFRESH_HOURS`); the release updates nightly.

**Season.** `CFB_SEASON` defaults to the current year from August on. Calibration reruns automatically for a new season or when `PC_VERSION` changes in `predict_cfb.R`.

## Where the data comes from

Everything except the upcoming schedule comes from cfbfastR's public release files, the same files `cfbfastR::load_cfb_pbp()` and friends read. As in the WBB build, the script fetches them directly so a change in the package's functions can't break it. None of them need an API key.

| Data | cfbfastR equivalent |
|---|---|
| Play-by-play with EPA, success, drives, players, game info and betting lines | `load_cfb_pbp()` |
| Final scores | `load_espn_cfb_schedules()` |
| 247 team talent composite | `load_cfb_team_talent()` |
| Returning production | `load_cfb_returning_production()` |
| Every-division schedules and scores, venues, playoff rounds | `cfbd_game_info()` release (`cfb_schedules`) |
| Transfer portal, team level (transfers in and out, 247 points) | `cfb_team_portal` release, 2015 on |
| Home stadium coordinates | `cfbd_team_info()` release (`cfb_team_info`) |

**FBS rankings, every FCS score.** Every game with at least one FBS team is used, and FCS-vs-FCS final scores (about 690 a season) join the points rating so FCS teams are rated from their whole season, not one or two FBS games. FCS teams get ratings so those games can be predicted, but they don't appear in the rankings. Membership comes from the play-by-play's division field each season.

**Portal data lags.** The team portal file for a season is published after the fact; at the time of writing there is none for 2026. With no file, every team's portal inputs are set to the FBS average, so the 2026 preseason prior is running without a term it was trained with.

**The schedule** comes first from ESPN, which carries lines. The build reads the whole regular season from ESPN's public scoreboard (16 requests, one per week), with ESPN's line where it has one. The rest of the season feeds the simulation, and the next 9 days (`UPCOMING_DAYS`) make the weekly slate. If ESPN fails and you have `cfbfastR` installed with a `CFBD_API_KEY` set, it uses CollegeFootballData instead: one call for the schedule plus one per upcoming week for lines. If both fail, it falls back to the public CollegeFootballData schedule release (`cfb_schedules`, no key, refreshed nightly upstream): the slate and simulation still run, but the slate has no market lines. If that fails too, the build finishes without a slate or simulation; the matchup tool still works. ESPN's team list supplies colors and abbreviations, and logos load from ESPN's CDN when the page is opened (initials show if they can't).

**Your API key is used only for 2025 and 2026.** Every call goes through one gate, `cfbd_call()` in `build_cfb.R`, which refuses any season not in `CFBD_SEASONS` (set to 2025 and 2026). All history, the backtest and calibration come from the public release files and never use the key. Each call is logged as it happens, and the build ends with a count. In practice the count is usually zero: the key is only a fallback, used when ESPN's scoreboard fails (schedule, one call, plus one call per upcoming week for lines), or before a season's first game (rosters, one call). Nothing currently needs 2025. To allow a new season later, add it to `CFBD_SEASONS`.

**These two ESPN pieces are untested.** They couldn't be reached from the machine this was built on, so the parsing is written defensively but has never seen a live response. If the slate comes up empty or lines look flipped, `fetch_espn_upcoming()` in `build_cfb.R` is the place to look.

## Method

1. **Plays.** Scrimmage plays only. Garbage time is removed using Bill Connelly's cutoffs: a margin over 43 points in the 1st quarter, 37 in the 2nd, 27 in the 3rd or 21 in the 4th. Overtime is never garbage time.
2. **Four ratings per team, offense and defense.** Success rate, EPA per play, points per game and plays per game (pace), each opponent- and home-field-adjusted. They are fitted by ridge regression on this season's games and pulled toward a preseason prior; the prior's weight, in games, is chosen by the backtest. The league average is pulled toward the last two seasons' average so week 1 numbers are sensible. The points rating uses every game including FCS-vs-FCS, so FCS teams no longer need a heavier prior.
3. **Preseason prior.** Fitted on earlier seasons only. Inputs are each team's ratings from the last two seasons, returning production on that side of the ball (also interacted with last season's rating), the talent composite, the transfer portal (247 points in and out, and the share of the roster that came through it) and the roster terms below. A team new to FBS starts at the 15th percentile. FCS teams get their own small regression on their last two seasons.
4. **From ratings to a game.** The predicted margin is home field plus the gap in adjusted success rate and the gap in adjusted points. The weights are fitted by regression on earlier seasons' week-by-week predictions, and they change with how many games the two teams have played: success rate counts a little more early, points a little less. Games with an FCS team get their own weights, which lean almost entirely on points, because an FCS team's success rate comes from one or two games. The prior weights and margin weights are chosen on FBS-vs-FBS error.
   - EPA per play is left out of the margin. It is about 0.95 correlated with the other two and got a negative weight when included. Dropping it changed the out-of-sample miss from 12.857 to 12.846 points, which is nothing.
   - It is still shown on the dashboard and used for totals, which come from a second regression on the points, EPA and pace ratings (including EPA times pace: efficiency matters more in a fast game).
5. **Win chance.** A normal model on the margin. The spread is fitted by maximum likelihood on earlier seasons, separately for weeks 1–4 and later weeks. Playoff games use the in-season spread: the teams are at full strength. Other bowls get a wider spread, the in-season spread scaled by how much bigger the misses on the margin are in bowls (opt-outs and coaching changes). There are only about 35 of those a season in the play-by-play (2023 on), too few to fit the spread on wins and losses alone; that estimate runs off past 60 points.
6. **Net, Off and Def on the rankings** put the margin formula in points. Off is points scored against an average defense, Def is points allowed to an average offense, and Net is Off minus Def: points better than an average FBS team on a neutral field.

## Player model (`players_cfb.R`)

Each season is fitted on its own from every non-garbage scrimmage play.

- **Passers and ball carriers.** A ridge regression splits each play's EPA:
  - pass play: league average + passer + pass defense;
  - run play: league average + ball carrier + the team's run game + run defense.

  Sacks count against the passer. The team run term absorbs blocking and scheme, so a back gets credit only for what the team's other runs don't explain. That term is also shown on team pages as "run game around the ball carrier", the closest public stand-in for offensive line play.
- **Receivers** are a second step. Each target's EPA, minus what that passer and that defense produce on an average throw, is averaged per receiver and shrunk toward zero.
- **Missing attribution.** cfbfastR records the target on nearly every completion but on far fewer incompletions (under 40% in 2022–2024). Throws with no recorded target are spread over the team's receivers by their recorded share, and throws with no recorded passer over that game's passers. Without this, every receiver looks better than he is.
- **No team pass-game term.** A term like the run game's, to split a passer from his line and receivers, was built and tested (`tune_team_pass()`) and is off: the stronger it was, the worse a passer's number predicted his next season, for the 98 who changed teams (correlation 0.16 without it, 0.02 at full strength) and the 529 who stayed (0.37 to 0.17).
- **Shrinkage.** The penalties for passers, backs, team run game and defense (150, 120, 200 and 200 plays) were chosen by fitting half of each season's games and predicting the other half (2023–2025).
- **Receivers are noisy.** Split-half reliability across 2016–2025 receivers is only 0.22, which sets the receiver shrinkage at 150 targets. Most of a receiver's value comes from volume.
- **Value per game** is the adjusted rate times plays per game, so it rewards both efficiency and usage.
- **What isn't rated.** Offensive linemen and most defenders have no play-level attribution in public data. Defenders get playmaking counts (sacks, interceptions, breakups, forced fumbles), and that attribution is itself patchy in some seasons.

**Projections.** Next season's rate comes from the last two seasons. The fit uses every season-to-season pair, weighted by plays, and the correlation with the following season is about 0.33 for all three roles. Players with no earlier FBS snaps start at the average first-year player in their role.

## Roster terms in the preseason prior

Five terms join the offensive side of the preseason regression, all fitted only on earlier seasons:

- the projected starting QB's rate;
- whether he has no FBS history;
- projected value of the backs and receivers on the roster;
- the change in QB from last season's starter;
- the change in the skill group from last season's.

**Roster sources.** Once a season starts, the roster is everyone who touched the ball in a team's first three games, and the starter is the dropback leader in the first two. **In the backtest each week uses only what was known before it**: the roster and starter come from that team's earlier games (up to three, and two for the starter), and before its first game, from last season's players. (c2.1 used the first three games for every week, which let weeks 1–3 see players and starters that hadn't played yet.) Before a season's first game, rosters come from `cfb_roster_overrides.csv`, then CollegeFootballData (with a key), then last season's players as a fallback; the dashboard warns when it falls back. `starting_qb` rows in the overrides file name each team's starter.

**What it bought.** Less than c2.1 reported. With as-of rosters, switching the roster terms off and rerunning the 2018–2025 backtest:

| | Without roster terms | With |
|---|---|---|
| Average miss, all lined games | 12.624 | 12.605 |
| Weeks 1–4 | 12.984 | 12.960 |
| Seasons where it helped | | 5 of 8 (2019 and 2020 were worse) |

That is 0.02 points per game (SE 0.012), not significant. c2.1's "better in every season, most in weeks 1–4" came from reading the roster out of games that hadn't been played yet. The terms stay on: they cost nothing, and live builds with a real preseason roster (the overrides file or CollegeFootballData) know more in week 1 than the backtest's last-season fallback does.

The QB change term carries most of it. A new starter projected 0.10 EPA per dropback better than last year's adds about 0.9 points per game to the preseason offense (points component).

## Season simulation (`simulate_cfb.R`)

The rest of the season is played 10,000 times (`SIM_N`). Each run draws every team's true strength around its rating:

- **Uncertainty.** The spread starts at 8.1 points, the measured spread of preseason misses, and narrows with games played the way the prior fades.
- **Games.** Each remaining game, title game and playoff game is a draw around the model's margin. The noise is reduced so that rating uncertainty isn't counted twice.

What it can't know, it stands in for:

- **Title games:** the top two in conference winning percentage. Ties go to more overall wins, then to the stronger team in that run. Real tiebreakers aren't modelled. A title game already on the schedule with both teams set is used as is.
- **Playoff selection:** a résumé score, strength in that run plus 6 points per game over .500 (`SIM_WIN_PTS`, hand-set). The five best-scoring conference champions get automatic bids and the next seven by score get at-large bids. Seeding is straight by score, the rule since 2025; seeds 1–4 get byes and seeds 5–8 host round one. The 12-team format is confirmed for 2026–27.
- **Bowls** other than the playoff aren't simulated.

**Check on 2025 (measured with c2.1, not yet rerun on c3.0).** Replayed from week 6, ten of the twelve eventual playoff teams were in its top twelve by playoff chance. The two it missed were the Group of Five champions, James Madison (16th) and Tulane (42nd); conference upsets are the hardest thing to see coming. Projected final win totals missed by 1.3 wins on average.

## Backtest

Every season from 2018 to 2025 was predicted week by week using only earlier games that season, earlier seasons, and the rosters known at the time. That includes the prior regressions, the prior weights and the margin weights, which were all re-chosen inside each season's fold. The market is the betting line carried in cfbfastR's play-by-play (from CollegeFootballData).

| | Model | Closing line |
|---|---|---|
| Winners picked (the 6,615 games with a line) | 76.1% | 76.0% |
| Average miss on margin, points | 12.61 | 12.21 |
| Average miss on total, points | 13.04 | 12.67 |
| Against the spread | 50.4% | |
| Log loss on win chances | 0.477 | 0.472 |

The closing line's log loss reads the line as a win chance with the model's own spread. Win chances are well calibrated: games given 70–80% were won 76% of the time. Games given 60–70% were won 68% of the time, a little more often than predicted.

| | Model | Closing line |
|---|---|---|
| Weeks 1–4 | 12.96 | 12.32 |
| Week 5 on | 12.41 | 12.16 |
| Playoff (25 games) | 10.42 | 11.22 |
| Other bowls (108 games) | 13.83 | 12.23 |

**Most of the remaining gap to the market is early in the season and in bowls.** Early on, it's information the market has and public play-by-play doesn't: depth charts, injuries, and how good a transfer will be in a new system. In bowls it's opt-outs and coaching changes. Against the spread the model is at a coin flip; it is a ratings model, not a betting edge.

The Report card tab has these numbers by season and by point in the season, the calibration chart, the live settings, and the settings each backtest season used.

## What changed in c3.0

A review of c2.1 proposed ten changes. Each is a switch in `CFG` at the top of `predict_cfb.R`, and each was tested by switching it off alone and rerunning the 2018–2025 backtest. Only the ones that helped are on; the others stay in the code, switched off, with the test result in a comment. Numbers below are the change in average miss on the margin (points, lined games) when the item is switched off; positive means it was helping. Standard errors, in brackets, are clustered by week.

| Change | Result | Status |
|---|---|---|
| FCS-vs-FCS scores in the points rating | +0.15 (0.03); games with an FCS team go from 0.96 to 2.07 points behind the market without it | On |
| Separate margin weights for games with an FCS team | +0.04 (0.02); weeks 1–4 FBS games +0.19 (0.05) | On |
| FCS prior fitted on FCS teams | +0.03 (0.01) | On |
| FCS prior weight back to 3× | +0.01 (0.01) | Off (1×) |
| Prior weights chosen on FBS-vs-FBS error | 0.00 | On (no cost) |
| As-of rosters in the backtest | −0.01 (0.01): the leak made c2.1 look slightly better | On, for honesty |
| Points per drive as a third margin rating | none: the backtest was 0.01 (0.01) better without it; used in place of points per game it was 0.23 (0.04) worse | Off |
| Playoff split from other bowls | postseason log loss 0.692 to 0.671 | On |
| Per-team prior weight from a model of prior error | 0.00 | Off |
| Prior weight estimated directly (empirical Bayes) | worse by 0.03 (0.02) | Off (grid search) |
| Game weight growing with the square root of plays | worse by 0.01 (0.01) | Off (one-for-one) |
| In-season recency decay, 8-week half-life | 0.01 (0.01) better, within noise; bowls worse | Off |
| Transfer portal in the prior | weeks 1–4 FBS games +0.10 (0.05) | On |
| Team pass-game term in the passer model | see the player model | Off |
| Home field from recent seasons, 2020 left out | 0.00: the margin regression fits its own home field | Off |
| Travel distance | 0.00 | Off |
| Margin weights that change with games played | weeks 1–4 FBS games +0.06 (0.03) | On |
| Pace in the totals | totals miss 0.04 lower | On |

Together, against c2.1 on the same games: 0.18 points per game better overall (0.04), 0.06 better on FBS-vs-FBS games (0.03), 0.27 better in weeks 1–4 FBS games (0.08) and about 1.1 better in games with an FCS team. The gap to the closing line went from 0.58 to 0.40 points per game. On 2026's first 331 lined games, c3.0 misses by 12.69 against c2.1's 13.05 (the market: 11.61).

## Traditional stats and the matchup screen (c3.1)

**Traditional stats** sit beside the advanced numbers everywhere. The Rankings tab has Offense stats and Defense stats views; team pages and the Players tab have an Advanced / Traditional switch (it carries across pages); every player with a box-score line has a page with his season and a game-by-game log, including defenders and kickers the model doesn't rate.

They are counted from the play-by-play in `prep_season()` (the `box` and `pbox` tables), from every scrimmage play with garbage time included, the way box scores count. Small differences from official totals are normal. Checked on 2024: Ashton Jeanty 371 carries, 2,589 yards, 29 TD (official 374, 2,601, 29); Kaleb Johnson 237, 1,556, 21 (official 240, 1,537, 21); Cam Ward 4,378 passing yards (official 4,313) but 35 TD against 39, because some scoring plays don't name the passer.

How each number is built, where the raw data needed help:

- **Sacks count against passing** (NFL style). The NCAA counts sack yards as rushing, so a quarterback's official rushing total is lower than his line here, which leaves sacks out.
- **First downs and third-down conversions.** The play-by-play's `firstD_by_yards` flag is almost never set, so a first down is a play that gained the distance without a turnover; a conversion also counts a touchdown. Penalty first downs are added to the team total.
- **Missing player ids.** About 9% of pass plays have no passer id but do have the passer's name parsed from the play text; the id is filled from other plays in the same game where that name and an id appear together. The same is done for ball carriers and receivers, and touchdown plays fall back to the scorer's id.
- **Red zone** is drives that reached the opponent's 20; scored means any points, TD means six or more. **Time of possession** is the sum of drive clocks.
- **Punt distance** is read from the play text (two styles, "punt for 52 yds" and "punt 36 yards"), because `yds_punted` is empty on most punts. **Kickers** are grouped by team and name, because the kicker id is missing on over half of field goal attempts.
- **Not available:** tackles (not in the public play-by-play), penalties by team (the play-by-play doesn't say which team was flagged), extra points, and targets (the intended receiver is recorded on fewer than half of incompletions, so only catches are shown). Defensive credits (sacks, interceptions, breakups, forced fumbles, recoveries) go to one player each and run below official totals.

**Defenders named only in the play text.** The 2026 play-by-play gives no player id for breakups (and for most forced fumbles); the play text names the player as "#14 T.Martin". `prep_season()` keeps those credits with the jersey and name, and the build matches each one to a player: first by team, jersey number and last name, then by team, first initial and last name when exactly one player fits. The pool to match against is the team rosters, from ESPN's public roster endpoint (one request per FBS team, cached for a day) or, if that fails and a key is set, one CollegeFootballData roster call; plus every defender credited with an id in the play-by-play this season and the two before. Checked on 2025, where most plays carry both the text and an id: the match agreed with the id on all 2,767 breakups and 94 forced fumbles it could check. Without a roster (as in this sample build, where ESPN couldn't be reached), about half of 2026's breakups match (1,076 of 2,154 credits); a roster with jersey numbers should cover nearly all of them. Credits that don't match stay in the Defense table under the name in the text ("S. Soles Jr."), with no player page. **The ESPN roster parsing is untested**, for the same reason as the scoreboard; the build log line "defensive credits named only in the text: X of Y matched (with ESPN rosters)" shows whether it worked.

Players with very small lines (under 5 pass attempts, 3 carries or 2 catches) are left out of the season tables, (c3.4 keeps every player's line in every game, in `gbox`, which feeds the recaps and the player pages' game logs).

**The matchup screen** opens from any upcoming game card on the Games tab, from a "Coming up" row on a team page, or from "Compare these teams" in the matchup predictor (any two teams, any site). It shows:

- **The prediction:** line, win chance, projected score and total, the market line and the gap to it, and each team's playoff chance before the game.
- **Where the edges are:** each offense against the other defense in ten areas (opponent-adjusted efficiency and success rate; run and pass EPA, big plays and finishing drives with garbage time removed; third downs, red zone, sack rate and turnovers from the box score). Each row shows both values with national ranks and a marker on a strip of field: like a drive, it moves toward the defense's end when the offense is stronger and back toward the offense's end when the defense is. The edge score is the gap in national percentile; under 0.15 is shown as even. Above the rows, up to four sentences name the biggest mismatches (gaps of 0.30 or more).
- **Tale of the tape:** ratings, scoring and yards, passing, rushing, situations and style, side by side with ranks and the better team marked.
- **Key players, recent form and common opponents.** Recent form shows each of the last five games against the model's pregame line.

An FCS opponent shows "not enough data" where the play-by-play only has its games against FBS teams.

## Games by week and game recaps (c3.4)

**The Games tab** is now a week at a time: a strip of weeks, with previous and next buttons, opens on the next week with games to play. A game still to play gets the matchup preview card and screen from c3.1; a finished game gets a recap card (final score, the model's pregame line and whether it picked the winner, the market line and who covered, how far the result was from the model line, and a small win-probability line) that opens the recap. Weeks beyond the 9-day slate list the rest of the schedule with the model's line and no market (ESPN carries lines only close to kickoff). A "Most dramatic first" sort orders finished games by how far the win probability moved. The week is in the address (`#games/5`), and Back from a recap or preview returns to that week.

**Recaps** (`#game/<id>`) are linked from every place a finished game appears: the Games tab, a team's game log, a player's game log, a matchup screen's recent form and common opponents, and the season strips at the bottom of each recap. Each shows:

- **The game:** score, line score by quarter and overtime, venue, attendance, bowl or kickoff-classic name and AP ranks (CollegeFootballData schedule release).
- **Win probability through the game.** Hover or tap to follow it. It starts at the model's pregame win chance and updates before every play: `P = Φ((lead + EP + line × r) / (σ √r))`, where lead is the home team's lead, EP the expected points of the possession (from the play-by-play), r the share of regulation left, and line and σ the model's pregame margin and single-game spread. This is Stern's random-walk model with the possession added. Scores and the clock come from the play-by-play (see below). On this build's 58,476 plays it scores a Brier of 0.079 against 0.112 for the play-by-play's own win probability column, which barely moves with the line (it gives a 28.5-point favorite 57% at kickoff). The build log prints both numbers. Overtime is drawn on a schematic axis, not a clock.
- **The result against expectations:** the model's line, the result against it, the market line and cover, the total against the model and the line, the winner's lowest win chance, lead changes, and the **postgame win expectancy**: a regression of every earlier season's final margin (2014 on) on the gap in EPA per play (every play), success rate and average starting field position (garbage time removed), plus home field. It explains 84% of margins with a residual spread of 9.0 points (9,953 games), and in development picked 89% of 2026's winners out of sample. It answers "how often does a team that played like this one win", which is not the same as who won.
- **What decided it:** up to five sentences chosen from the factors that separated the teams (efficiency, turnovers and points off them, big plays, finishing drives, field position, third downs, sacks, a comeback, garbage time, a surprise against the line). Efficiency leads; the rest are ranked by size.
- **Key plays:** the five plays that moved the win probability most (at least 3 points), numbered on the chart.
- **Team stats:** box score (every play) and efficiency (garbage time removed), the better side marked; each offense against its season average and against what the opponent's defense usually allows.
- **Scoring plays, drives and box scores:** scoring plays with the drive that produced them; every drive on a strip of field (the scoring ones in green); every player's line with EPA on his plays (passes, runs and catches, garbage time included, sacks charged to the passer).

**How the score is rebuilt.** The play-by-play's score columns are right on scoring plays and wrong on runs of other rows in some games (one game shows 23–23 in the first quarter), and a few rows carry the period or clock of a different part of the game. So the recap reads only the scoring plays: points only go up, so sorting them by total points puts them in order, a candidate that doesn't extend the chain or overshoots the official final is dropped, and kickoff rows (whose scores are the kicking team's) are flipped. Every other play takes the score after the last scoring play before it. Rows whose clock disagrees with the play-number order (719 of 68,636 plays in 2026, about 1%) are left off the timeline but kept in drive counts; timeouts, penalties and period markers are never placed. All 389 line scores add up to the official finals. Where the play-by-play is missing a scoring play, the points land on the next scoring play that is there: 7 games have one scoring step of 9 to 14 points (two scores' worth), so their line score puts both in the later period. One game ends 6 points short of its official score; those points are added to the last period and the recap says so under the line score.

**Cost.** The first build after updating reads the 2026 play-by-play once more (about 10 seconds) and caches it in `.cfb_cache/recap_2026_r1.rds`; the cache refreshes with the play-by-play. The dashboard file grows from about 2.2 MB to about 4.3 MB, and grows through the season (about 0.005 MB per game). Past seasons are not reprocessed. Recaps cover the current season only.

**Limits.** Win probability is a model, not the play-by-play's column and not a market: it knows nothing about injuries, timeouts or weather. It is well calibrated in the middle and a little overconfident at the extremes: in development, plays it put under 10% (mostly late in lopsided games) were won 5.8% of the time against 2.5% predicted. Defenders credited only in the play text and not matched to a player show as "Name not in the data" unless the ESPN roster fetch worked. Tackles are not in the public data.

## The predictions log

Each build writes every upcoming game's line, win chance and total to `predictions_log_cfb.csv`, stamped with the time. A pick is overwritten on later builds only until its game kicks off, so the log holds the last pick made before kickoff. On the Games tab, a finished game's model line is the saved pick when the log has one, and otherwise a replay: the model's numbers rebuilt from the games before that week with the same settings. The "The model this season" counts show how many are saved.

## What it doesn't do yet

- **Linemen and defenders.** Not rated (no play-level data). Team run game is the stand-in for the line.
- **Recruits.** Freshmen and players with no FBS snaps start at the average newcomer. Recruiting rank by player isn't used yet; the team talent composite covers it only in aggregate.
- **Injuries and quarterback availability.** There's no feed; a hand-kept file like `wbb_injuries.csv` would work here too, using the player values to price a missing starter.
- **Weather and rest.** Not modelled; no public weather feed was found in the release files. Travel distance was tested and added nothing.
- **Coaching changes and preseason market win totals.** No public source in the release files. Both would most help weeks 1–4.

## c3.4.1 patch notes

- **Player stats in previews and recaps.** The matchup preview's Key players and the recap's new Player stats section have the same Advanced / Traditional switch as the team page (it carries across pages). Traditional is the team page's stat leaders. Advanced is its leaders by EPA per play and success rate. In a recap the switch also changes the box score: Traditional is the game's lines, Advanced is plays, EPA per play and success rate for each passer, runner and receiver in that game. Season leaders for both teams sit below the box score.
- **"Where the edges were" in recaps.** The ten matchups from the preview, each offense against the other defense, with what happened. The solid marker is the offense's number in this game placed among every team-game of the season (counted from `gteam`, nothing new to build); the hollow marker is where the season numbers said it would go, as in the preview. Each row says who won the matchup and whether that was as expected or against the numbers; above the rows, a line counts the 20 matchups and up to three of the biggest reversals are named. Rows with too few plays (under 20 plays for efficiency, 8 rushes or dropbacks, 4 third downs, 2 red-zone trips) show "Not enough plays".
- **Rolling score in the drive tracker.** Each drive shows the score after it, away team first, with the side that scored highlighted. `recap_cfb.R` now ships it with every drive (two new columns at the end of each `dr` row); the page falls back to rebuilding it from the scoring plays when it's missing, so a cached or older build still works. On the 389 games in the sample the final drive's score matches the official final in 388; the other is the game the play-by-play is 6 points short on.
- **Key plays swing.** The "+" on each key play was the swing in the helped team's win chance, but the line under it showed the favourite's win chance before and the helped team's after, so the two often didn't match. It now shows the helped team's own win chance before and after, rounded as displayed, and the "+" is exactly the difference.
- Ratings, predictions, simulation and the report card are untouched. No cache needs rebuilding: `RECAP_VERSION` is unchanged.

## c3.4 patch notes

- New `recap_cfb.R`, `#game/<id>` recap screen, week-by-week Games tab, recap links everywhere a game appears, a full-schedule export (`schedule`), per-game team and player box scores (`gteam`, `gbox`, `pnames`).
- `prep_season()`'s player-id filling moved into `box_player_ids()` so recaps and the season box score count the same player on the same play. Checked on 2026: every table in `box`, `pbox` and `plays` is identical before and after, so `PREP_VERSION` is unchanged and past seasons are not reprocessed.
- `tlog` (c3.1's game logs for regulars) is replaced by `gbox`, which covers every player in every game and adds EPA.
- Ratings, predictions, simulation and the report card are untouched.
- Games tab: the old results table (with its Source column) is replaced by recap cards. Whether a game's model line was saved or replayed is in a tooltip on the card's model line; the totals are under "The model this season".

## c3.3 patch notes

Run end to end in R 4.x against real 2014-2026 data; backtest numbers are 2018-2025, 6,677 games. Baseline (original c3.0) reproduced the Report card exactly: 12.605 vs 12.210 market.

Defaults that shipped
- Sparse Cholesky solver in `fit_ratings` (same ratings to 1e-13, ~9x faster per fit).
- QB term (`CFG$qb_avail`): expected-starter gap vs the QBs behind the ratings so far. Gap to market 0.396 -> 0.377 (2023-25: 0.304 -> 0.269); paired gain 0.019 pts/game, 95% CI 0.002-0.037. Backtest uses the QB who played (starter known by kickoff), so it is a ceiling. Live: `cfb_availability.csv` (team_id, athlete_id, action = out | starting_qb), else the QB from the team's last game.
- Negative FCS-game quality weights zeroed (`CFG$fcs_nonneg`).
- Date-based as-of cutoffs, pregame-only pick logging with a `late` flag, simulation errors and missing portal data surfaced as warnings, passer shares renormalised.
- Template: sticky nav and table header, compact phone columns, 44px touch targets, AA-contrast tokens, wide layout above 1680px, "raw" tags on unadjusted matchup rows, JS matchup tool matches the R layer.

Prior weight (added after the first c3.3 pass)
- `k_mode = "uniform"` is now the default: one prior weight for every rating component, chosen walk-forward by margin error. It picks 4 games nearly every season. The old per-component grid drifted to the grid edge and swung between seasons (fold-to-fold SD of k 1.31 vs 0.35), for the same accuracy (gap to market 0.377 vs 0.375).
- A fixed 4 (`CFG$k_fixed = 4`) scored best in the backtest (0.369, better in weeks 1-4 and 5+, log loss and totals) but 4 was picked after looking at that same backtest, so it is not the default.
- The preseason prior is not overweighted: with one k for all components the optimum is 4 games overall and 4-8 in weeks 3-8, and halving it costs 0.2+ pts in those weeks. Same on 2026 (gap 0.89 at k=4, 1.03 at k=2).
- Roster-turnover-scaled prior weights (`CFG$team_prior_w`) were re-tested: slightly worse in every configuration. Prior misses are only 4-11% larger for high-turnover teams (new QB +6%, big QB change +4%, low returning production +11%, high portal share +5%), because the prior mean already adjusts for them.

Built, tested, and OFF because the backtest did not support them
- `fbs_level` (signed FBS-vs-FCS term): no gain; FCS bias flips sign by season.
- `fcs_sigma` (separate FCS spread): worse log loss (0.4773 -> 0.4780).
- `huber_p` (Huber points ratings): 1.0/1.5/2.5 gave gap 0.448/0.407/0.397 vs 0.396.
- `layer_decay`, `qb_dead`: no gain.

Open problem: FBS teams still beat the model in games against FCS teams (+1.4 pts on average, +4.1 in 2025, +5.95 so far in 2026). Nothing tested fixes it.

PC_VERSION is c3.3 and PL_VERSION is p1.2: the first build recalibrates.
