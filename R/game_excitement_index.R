#' Game Excitement Index
#'
#' @description Computes Game Excitement Index for Desired Game
#' @usage game_excitement_index(.data)
#'
#' @param .data play-by-play data frame returned from w_get_pbp_game function
#' @return GEI--Game Exictement Index
#' @export
game_excitement_index <- function(.data) {

  pbp_data <- .data
  ### Error Testing
  if(is.null(pbp_data)) {
    stop("pbp_data is missing with no default")
  }
  
  ### Compute Game Excitemant Index
  msec <- max(pbp_data$secs_remaining_absolute)
  pbp_data$wp_delta <- 0
  pbp_data$wp_delta <- abs(pbp_data$win_prob - lag(pbp_data$win_prob))
  gei <- sum(pbp_data$wp_delta, na.rm = TRUE) * 2400/msec
  return(gei)
}
