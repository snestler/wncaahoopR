#' Get Team Play-by-Play Data
#'
#' Scrapes the current season's Play-by-Play data for desired team. Team
#' is assumed to be the ESPN team name, which can be looked up in the ids
#' dataframe.
#' @description Scrapes ESPN Play-by-Play data for a team's entire season.
#' @usage w_get_pbp_season(team)
#'
#' @param team Team to get Play-by-Play data for
#' @return A list containing a data frame for each game. Returns a message if no game data is available.
#' @export
w_get_pbp_season <- function(team) {
  ### Error Testing
  if(is.null(team)) {
    stop("team is missing with no default")
  }
  if(!team %in% ids$team) {
    stop("Invalid team. Please consult the ids data frame for a list of valid teams, using data(ids).")
  }

  message(paste("Getting Game IDs: ", team, sep = ""))

  ### Get Game IDs
  game_ids <- w_get_game_ids(team)

  ### Get PBP Data
  pbp_season <- lapply(game_ids, function(x) w_get_pbp_game(x))

  return(pbp_season)
}
