#' Win Probability Function

#' @description Computes the win probability, given a game state.
#' @usage wp_compute(x)
#' @return A single number, a probability between 0 and 1.
#' 
#' @export
#' @keywords internal

wp_compute <- function(x) {
  # load("R/sysdata.rda")
  score_diff_smooth <-
    loess(estimate ~ max_time,
          data = filter(coeffs, coefficient == "score_diff"),
          span = 0.5)
  
  favored_by_smooth <-
    loess(estimate ~ max_time,
          data = filter(coeffs, coefficient == "favored_by"),
          span = 0.5)
  
  if(is.na(x$home_favored_by[1])) {
    x$home_favored_by <- 0
  }
  ### Get Coefficient Values for Current Game
  sc_diff <- predict(score_diff_smooth, newdata = x$secs_remaining_relative)
  fb <- predict(favored_by_smooth, newdata = x$secs_remaining_relative)
  
  ### Capture Game Determinism
  index <- x$secs_remaining == 0 & (x$home_score != x$away_score)
  sc_diff[index] <- 20
  fb[index] <- predict(favored_by_smooth, newdata = 1)
  
  ### Compute log odds of winning
  log_odds <-
    sc_diff * x$score_diff  +
    fb * x$home_favored_by
  
  
  return(logit(log_odds))
}
