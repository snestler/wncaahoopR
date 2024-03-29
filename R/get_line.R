#' Get Line
#'
#' @description Gets the line (in points) of the game from WHERE?
#' @usage get_line(.data)
#' 
#' @param .data play-by-play data frame returned from w_get_pbp_game function
#' @return Line
#' @importFrom dplyr filter
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_text
#' @export
### Impute Line for Games


get_line <- function(.data) {

  pbp_data <- .data
  ### Error Testing
  if(is.null(pbp_data)) {
    stop("pbp_data is missing with no default")
  }
  
  game_date <- pbp_data$date[1]
  away <- pbp_data$away[1]
  home <- pbp_data$home[1]
  
  ### Convert to NCAA Names
  away <- dict$NCAA[dict$ESPN_PBP == away]
  home <- dict$NCAA[dict$ESPN_PBP == home]
  
  ### Get Predicted Line
  if(length(home) == 0 | length(away) == 0) {
    return(NA)
  }
  
  ### Don't have Imputed Lines Before 2016-17
  if(game_date < "2016-11-01") {
    return(NA)
  }
  
  ### Impute from 2016-17 Season
  if(game_date >= "2016-11-01" & game_date <= "2017-05-01") {
    game <- filter(games_2016, team == home, opponent == away, date == game_date)
    return(ifelse(nrow(game) > 0, game$pred_score_diff[1], NA))
  }
  
  ### Impute from 2017-18 Season
  if(game_date >= "2017-11-01" & game_date <= "2018-05-01") {
    game <- filter(games_2017, team == home, opponent == away, date == game_date)
    return(ifelse(nrow(game) > 0, game$pred_score_diff[1], NA))
  }
  
  ### Impute from 2018-19 Season
  if(game_date >= "2018-11-01" & game_date <= "2019-05-01") {
    game <- filter(games_2018, team == home, opponent == away, date == game_date)
    return(ifelse(nrow(game) > 0, game$pred_score_diff[1], NA))
  }
  
  return(NA)
}
