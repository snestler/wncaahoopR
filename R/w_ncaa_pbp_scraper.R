# source("R/w_helpers.R")
# ^^^^^ Commented this out to prevent problem when building package.


###################################Get Season Long PBP Data ####################
#' Get Team Play-by-Play Data
#'
#' Scrapes the current season's Play-by-Play data for desired team. Team
#' is assumed to be the ESPN team name, which can be looked up in the ids
#' dataframe.
#'
#' @param team Team to get Play-by-Play data for
#' @return A data-frame of the team's Play-by-Play data for the current season
#' @export
get_pbp <- function(team) {
  ### Error Testing
  if(is.na(team)) {
    stop("team is missing with no default")
  }
  if(!"wncaahoopR" %in% .packages()) {
    ids <- create_ids_df()
  }
  if(!team %in% ids$team) {
    stop("Invalid team. Please consult the ids data frame for a list of valid teams, using data(ids).")
  }

  message(paste("Getting Game IDs: ", team, sep = ""))

  ### Get Game IDs
  game_ids <- get_game_ids(team)

  ### Get PBP Data
  pbp_season <- get_pbp_game(game_ids)

  return(pbp_season)
}

################################  Get Schedule #################################
#' Get Team Schedule
#'
#' Gets team schedule for current season.
#'
#' @param team Team to get schedule for
#' @return A data-frame of the team's schedule for current season
#' @export
w_get_schedule <- function(team) {
  ### Error Testing
  if(is.na(team)) {
    stop("team is missing with no default")
  }
  if(!"wncaahoopR" %in% .packages()) {
    ids <- create_ids_df()
  }
  if(!team %in% ids$team) {
    stop("Invalid team. Please consult the ids data frame for a list of valid teams, using data(ids).")
  }

  ### Scrape Team Schedule
  base_url <- "https://www.espn.com/womens-college-basketball/team/schedule/_/id/"
  url <- paste(base_url, ids$id[ids$team == team], "/", ids$link[ids$team == team], sep = "")
  schedule <- XML::readHTMLTable(RCurl::getURL(url))[[1]][-1,]
  schedule <- schedule[,1:4]
  names(schedule) <- c("date", "opponent", "result", "record")
  schedule <- schedule[!is.na(schedule$opponent) & schedule$opponent != "Opponent",]

  ### Locations
  schedule$location <- ifelse(grepl("[*]", schedule$opponent), "N",
                              ifelse(grepl("^vs", schedule$opponent), "H", "A"))

  ### Clean Opponent Names
  schedule$opponent <- gsub("^vs", "", schedule$opponent)
  schedule$opponent <- gsub("[@#*()]", "", schedule$opponent)
  schedule$opponent <- gsub("[0-9]*", "", schedule$opponent)
  schedule$opponent <- stripwhite(gsub("^ ", "", schedule$opponent))

  ### Scores/Results
  schedule$result[grep(":", schedule$result)] <- NA
  schedule$result[grep("TBD", schedule$result)] <- NA
  scores <- unlist(sapply(gsub("[A-z]*", "", schedule$result), strsplit, "-"))
  scores <- gsub("\\s.*", "", scores)
  team_scores <- suppressWarnings(as.numeric(scores[seq(1, length(scores), 2)]))
  opp_scores <- suppressWarnings(as.numeric(scores[seq(2, length(scores), 2)]))
  schedule <- dplyr::mutate(schedule, team_score = NA, opp_score = NA)
  schedule$team_score[1:length(team_scores)] <- team_scores
  schedule$opp_score[1:length(opp_scores)] <- opp_scores
  index <- grepl("L", schedule$result)
  tmp <- schedule$team_score[index]
  schedule$team_score[index] <- schedule$opp_score[index]
  schedule$opp_score[index] <- tmp

  ### Dates
  schedule$day <- as.numeric(gsub("[^0-9]*", "", schedule$date))
  schedule$month <- substring(schedule$date, 6, 8)
  schedule$month[schedule$month == "Nov"] <- 11
  schedule$month[schedule$month == "Dec"] <- 12
  schedule$month[schedule$month == "Jan"] <- 1
  schedule$month[schedule$month == "Feb"] <- 2
  schedule$month[schedule$month == "Mar"] <- 3
  schedule$month[schedule$month == "Apr"] <- 4
  schedule$month <- as.numeric(schedule$month)
  schedule$year <- ifelse(schedule$month <= 4, 20, 19)
  schedule$date <- paste(schedule$month, schedule$day, schedule$year, sep = "/")

  ### Game IDs
  schedule$date <- as.Date(schedule$date, "%m/%d/%y")
  schedule <- dplyr::arrange(schedule, date)
  schedule$game_id <- get_game_ids(team)

  ### Return Schedule
  return(schedule[,c("game_id", "date", "opponent", "location",
                     "team_score", "opp_score", "record" )])
}

######################### Get Game IDs ########################################
#' Get Team game_ids
#'
#' Gets team game_ids for current season.
#'
#' @param team Team to get game_ids
#' @return A vector of the team's ESPN game_ids for current season
#' @export
w_get_game_ids <- function(team) {
  ### Error Testing
  if(is.na(team)) {
    stop("team is missing with no default")
  }
  if(!"wncaahoopR" %in% .packages()) {
    ids <- create_ids_df()
  }
  base_url <- "http://www.espn.com/womens-college-basketball/team/_/id/"
  url <- paste(base_url, ids$id[ids$team == team], "/", ids$link[ids$team == team], sep = "")

  x <- scan(url, what = "", sep = "\n")
  x <- x[grep("club-schedule", x)]
  x <- unlist(strsplit(x, "gameId="))
  x <- x[-1]
  x <- x[1:(floor(length(x)/2))]
  reg_flag <- grep("<h2>Regular Season</h2>", x)

  game_ids <- substring(x, 1, 9)
  if(length(reg_flag) > 0) {
    game_ids <- c(game_ids[-c(1:reg_flag)], game_ids[1:reg_flag])
  }
  game_ids <- unique(game_ids)

  return(game_ids)
}


####################### Function To Get a Schedule by Date #####################
#' Get Master Schedule
#'
#' Gets schedule for all games on a given date
#'
#' @param year year for which to get master schedule
#' @param month month for which to get master schedule
#' @param day day for which to get master schedule
#' @return A data-frame of the day's schedule of games
#' @export
w_get_master_schedule <- function(year, month, day) {
  ### Error Testing
  if(is.na(year)) {
    stop("year is missing with no default")
  }
  if(is.na(month)) {
    stop("month is missing with no default")
  }
  if(is.na(day)) {
    stop("day is missing with no default")
  }

  tmp <- try(as.Date(paste(year, month, day, sep = "-")))
  if(class(tmp) == "try-error") {
    stop("Please enter valid date")
  }

  date <- paste0(year, ifelse(nchar(month) == 1, paste0("0", month), month),
                 ifelse(nchar(day) == 1, paste0("0", day), day))
  url <- paste0("https://www.espn.com/womens-college-basketball/schedule/_/date/", date)
  z <- XML::readHTMLTable(RCurl::getURL(url))
  if(length(z) > 1) {
    schedule <- as.data.frame(z[[1]])[,c(1,2)]
    completed <- as.data.frame(z[[2]][-1,1:3])
    names(completed) <- c("away", "home", "result")
    names(schedule) <- c("away", "home")
  }else{
    completed <- as.data.frame(z[[1]][,1:3])
    names(completed) <- c("away", "home", "result")
    schedule <- NA
  }

  n_canceled <- sum(grepl("Canceled", completed$result))
  n_postponed <- sum(grepl("Postponed", completed$result))
  completed <- dplyr::filter(completed, result != "Canceled", result != "Postponed")

  ### Extract Ranking
  ranking <- function(team) {
    rank <- gsub("[^0-9]", "", team)
    return(ifelse(rank == "", NA, rank))
  }

  ### Clean Team Name
  clean <- function(team) {
    team <- gsub("[#0-9]", "", team)
    team <- gsub("\\s[A-Z]*-*[A-Z]*$", "", team)
    team <- gsub("TA&M", "", team)
    team <- gsub("\\s*$", "", gsub("^\\s*", "", team))
    return(team)
  }

  if(any(!is.na(schedule[1]))) {
    schedule <- dplyr::mutate(schedule,
                              "away" = as.character(sapply(schedule$away, clean)),
                              "home" = as.character(sapply(schedule$home, clean)),
                              "away_rank" = as.numeric(sapply(schedule$away, ranking)),
                              "home_rank" = as.numeric(sapply(schedule$home, ranking)),
                              "away_score" = NA,
                              "home_score" = NA)
  }

  x <- scan(url, sep = "\n", what = "")
  x <- x[grep("gameId", x)[1]]
  x <- gsub("[A-z]", "", x)
  x <- strsplit(x, "\\?=")[[1]]
  x <- suppressWarnings(as.numeric(unname(sapply(x, function(y){ substring(y, 1, 9) }))))
  x <- x[!is.na(x) & !duplicated(x)]
  x <- x[1:(length(x) - n_canceled - n_postponed)]

  ### Add in Completed Games
  find_anchor <- function(team) {
    cleaned <- clean(team)
    team <- gsub("[#0-9]", "", team)
    team <- gsub("\\s*$", "", gsub("^\\s*", "", team))
    anchor <- unlist(strsplit(team, ""))[-c(1:(nchar(cleaned) + 1))]
    return(paste0(anchor, collapse = ""))
  }

  completed <- dplyr::mutate(completed,
                             "away" = as.character(sapply(away, clean)),
                             "home" = as.character(sapply(home, clean)),
                             "result" = as.character(result),
                             "away_rank" = as.numeric(sapply(completed$away, ranking)),
                             "home_rank" = as.numeric(sapply(completed$home, ranking)),
                             "away_anchor" = sapply(completed$away, find_anchor),
                             "away_score" = NA,
                             "home_score" = NA)

  winners <- unname(sapply(completed$result, function(y) { gsub("\\s[0-9]*.*", "", y) }))
  scores <- as.numeric(gsub("[^0-9]", "", gsub("\\(.*\\)", "", unlist(strsplit(completed$result, ",")))))

  if(length(scores) > 0) {
    winning_scores <- scores[seq(1, length(scores) - 1, 2)]
    losing_scores <- scores[seq(2, length(scores), 2)]

    index <- sapply(completed$away_anchor, function(y) { y %in% winners })
    completed$home_score[index] <- losing_scores[index]
    completed$home_score[!index] <- winning_scores[!index]
    completed$away_score[!index] <- losing_scores[!index]
    completed$away_score[index] <- winning_scores[index]
  }

  if(any(!is.na(schedule[1]))) {
    schedule <- rbind(schedule, dplyr::select(completed, -away_anchor, -result))
  }else{
    schedule <- completed
  }

  schedule <- dplyr::mutate(schedule, "game_id" = x)
  schedule <- dplyr::select(schedule, game_id, away, home, away_rank, home_rank, away_score, home_score)

  return(schedule)
}
