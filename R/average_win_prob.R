#' Average Win Probability
#' 
#' @description This function returns the average win probability for the home team.
#' @usage average_win_prob(pbp_data)
#' 
#' @param .data play-by-play data frame returned from w_get_pbp_game function
#' @return Average Win Probability
#' @export
average_win_prob <- function(.data) {

  pbp_data <- .data
  ### Error Testing
  if(is.null(pbp_data)) {
    stop("pbp_data is missing with no default")
  }
  avg_wp <- sum(pbp_data$play_length * pbp_data$win_prob/max(pbp_data$secs_remaining_absolute), 
                na.rm = TRUE)
  return(avg_wp)
}
