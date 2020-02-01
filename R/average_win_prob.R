#' Average Win Probability
#' 
#' @description This function returns the average win probability for the home team.
#' @usage average_win_prob(pbp_data)
#' 
#' @param pbp_data Play-by-play data returned from w_get_pbp_game
#' @return Average Win Probability
#' @export
average_win_prob <- function(pbp_data) {
  ### Error Testing
  if(is.na(pbp_data)) {
    stop("pbp_data is missing with no default")
  }
  avg_wp <- sum(pbp_data$play_length * pbp_data$win_prob/max(pbp_data$secs_remaining_absolute), 
                na.rm = TRUE)
  return(avg_wp)
}
