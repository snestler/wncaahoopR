### Impute Line for Games
get_line <- function(data) {
  game_date <- data$date[1]
  away <- data$away[1]
  home <- data$home[1]
  
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
    game <- dplyr::filter(games_2016, team == home, opponent == away, date == game_date)
    return(ifelse(nrow(game) > 0, game$pred_score_diff[1], NA))
  }
  
  ### Impute from 2017-18 Season
  if(game_date >= "2017-11-01" & game_date <= "2018-05-01") {
    game <- dplyr::filter(games_2017, team == home, opponent == away, date == game_date)
    return(ifelse(nrow(game) > 0, game$pred_score_diff[1], NA))
  }
  
  ### Impute from 2018-19 Season
  if(game_date >= "2018-11-01" & game_date <= "2019-05-01") {
    game <- dplyr::filter(games_2018, team == home, opponent == away, date == game_date)
    return(ifelse(nrow(game) > 0, game$pred_score_diff[1], NA))
  }
  
  return(NA)
}
