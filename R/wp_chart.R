#' Win Probability Chart
#'
#' Renders win proability chart for desired game
#'
#' @param pbp_data Play-by-play data returned from w_get_pbp_game
#' @param home_col Color of home team for chart
#' @param away_col Color of away team for chart
#' @param show_legend Logical indicating whether or not to display legend and min win probability
#' on the chart. Default = TRUE
#' @export
wp_chart <- function(pbp_data, home_col, away_col, show_legend = TRUE) {
  ### Error Testing
  if(is.na(pbp_data)) {
    stop("game_id is missing with no default")
  }
  if(is.na(home_col)) {
    stop("home_col is missing with no default")
  }
  if(is.na(away_col)) {
    stop("away_col is missing with no default")
  }
  
  ### Scrape Data from ESPN
  date <- format(as.Date(pbp_data$date[1]), "%B %d, %Y")
  msec <- max(pbp_data$secs_remaining_absolute)
  
  ### Cleaning
  pbp_data$scorediff <- pbp_data$home_score - pbp_data$away_score
  if(is.na(pbp_data$home_favored_by[1])) {
    pbp_data$home_favored_by <- get_line(pbp_data)
  }
  if(!is.na(pbp_data$home_favored_by[1])){
    pbp_data$pre_game_prob <- predict(prior, newdata = data.frame(pred_score_diff = pbp_data$home_favored_by),
                                  type = "response")
  }else{
    pbp_data$pre_game_prob <- 0.5
  }
  
  ### Game Excitemant Index
  pbp_data$wp_delta <- 0
  for(i in 2:nrow(pbp_data)) {
    pbp_data$wp_delta[i] <- abs(pbp_data$win_prob[i] - pbp_data$win_prob[i-1])
  }
  gei <- sum(pbp_data$wp_delta, na.rm = T) * 2400/msec
  gei <- paste("Game Excitement Index:", round(gei, 2))
  gap <- 0.08
  
  ### Plot Results
  pbp_data$secs_elapsed <- max(pbp_data$secs_remaining_absolute) - pbp_data$secs_remaining_absolute
  title <- paste("Win Probability Chart for", pbp_data$away[1], "vs.", pbp_data$home[1],"\n", date[1])
  if(pbp_data$scorediff[nrow(pbp_data)] < 0) {
    plot(win_prob ~ secs_elapsed, data = pbp_data, col = home_col, type = "l", lwd = 3, ylim = c(0,1),
         xlab = "Seconds Elapsed", ylab = "Win Probability", main = title)
    par(new = T)
    plot((1 - win_prob) ~ secs_elapsed, data = pbp_data, col = away_col, type = "l", lwd = 3, ylim = c(0,1),
         xlab = "", ylab = "", main = "")
    abline(h = 0.5, lty = 2)
  }
  else{
    plot((1 - win_prob) ~ secs_elapsed, data = pbp_data, col = away_col, type = "l", lwd = 3, ylim = c(0,1),
         xlab = "Seconds Elapsed", ylab = "Win Probability", main = title)
    par(new = T)
    plot(win_prob ~ secs_elapsed, data = pbp_data, col = home_col, type = "l", lwd = 3, ylim = c(0,1),
         xlab = "", ylab = "", main = "")
    abline(h = 0.5, lty = 2)
  }
  if(show_legend) {
    gap <- 0.02
    if(pbp_data$win_prob[1] < 0.85) {
      legend("topleft", col = c(home_col, away_col), legend = c(pbp_data$home[1], pbp_data$away[1]), lty = 1,
             cex = 0.5)
    }
    else{
      legend("left", col = c(home_col, away_col), legend = c(pbp_data$home[1], pbp_data$away[1]), lty = 1,
             cex = 0.5)
    }
  }
  
  ### Min Win Prob
  if(pbp_data$score_diff[nrow(pbp_data)] > 0) {
    min_prob <- min(pbp_data$win_prob, na.rm = T)
    if(min_prob < 0.01) {
      min_prob <- paste("Minimum Win Probability for", pbp_data$home[1], "< 1", "%")
    }
    else{
      min_prob <- paste("Minimum Win Probability for", pbp_data$home[1], round(100 * min_prob, 1), "%")
    }
  }
  else{
    min_prob <- min(1 - pbp_data$win_prob, na.rm = T)
    if(min_prob < 0.01) {
      min_prob <- paste("Minimum Win Probability for", pbp_data$away[1], "< 1", "%")
    }
    else{
      min_prob <- paste("Minimum Win Probability for", pbp_data$away[1], round(100 * min_prob, 1), "%")
    }
  }
  if(show_legend) {
    text(600, 2 * gap, min_prob, cex = 0.8)
    text(600, gap, gei, cex = 0.8)
  }
}
