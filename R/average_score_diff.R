#' Average Score Differential
#'
#' @description Returns a single value for the average score differential from home team's perspective.
#' @usage average_score_diff(pbp_data)
#' 
#' @param .data play-by-play data frame returned from w_get_pbp_game function
#' @return Average score differential
#' @export
average_score_diff <- function(.data) {
  
  pbp_data <- .data
  ### Error Testing
  if(is.null(pbp_data)) {
    stop("game_id is missing with no default")
  }
  
  avg_sd <- sum(pbp_data$play_length * pbp_data$score_diff/max(pbp_data$secs_remaining_absolute), na.rm = T)
  return(avg_sd)
}
