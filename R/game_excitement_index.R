#' Game Excitement Index
#'
#' @description Computes Game Excitement Index for Desired Game
#' @usage game_excitement_index(pbp_data)
#'
#' @param pbp_data Play-by-play data returned from w_get_pbp_game
#' @return GEI--Game Exictement Index
#' @export
game_excitement_index <- function(pbp_data) {
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
