#' Win Probability Charts in ggplot
#' 
#' @description This function creates a win probability chart over the game duration.
#' @usage gg_wp_chart(pbp_data, home_col, away_col, show_labels)
#' 
#' @param pbp_data Play-by-play data returned from w_get_pbp_game
#' @param home_col Color of home team for chart
#' @param away_col Color of away team for chart
#' @param show_gei Logical whether Game Exictement Index and Minimum
#' Win Probability metrics should be displayed on the plot. Default = TRUE.
#' @details This function takes play-by-play data returned from w_get_pbp_game
#' and returns a ggplot2 object to show each teams win probability during the game.
#' @examples
#' pbp_data <- w_get_pbp_game("401176897")
#' gg_wp_chart(pbp_data, "red", "black")
#' @importFrom magrittr %>% 
#' @export
gg_wp_chart <- function(pbp_data, home_col = NULL, away_col = NULL, show_gei = TRUE) {
  
  if(is.na(pbp_data)) {
    stop("game_id is missing with no default")
  }
  if(is.null(home_col)) {
    home_col <- ncaa_colors$primary_color[ncaa_colors$espn_name == unique(pbp_data$home)]
  }
  
  if(length(home_col) == 0) {
    home_col <- "red"
    message("There were no colors found for the home team -- defaulting to red.")
  }
  
  if(is.null(away_col)) {
    away_col <- ncaa_colors$primary_color[ncaa_colors$espn_name == unique(pbp_data$away)]
  }
  
  if(length(away_col) == 0) {
    away_col <- "black"
    message("There were no colors found for the away team -- defaulting to black.")
  }
  
  ### Get Data
  home_team <- pbp_data$home[1]
  away_team <- pbp_data$away[1]
  plot_lines <- 1200
  msec <- max(pbp_data$secs_remaining_absolute)
  sec <- msec - 2400
  ot_counter <- 0
  while(sec > 0) {
    sec <- sec - 300
    plot_lines <- c(plot_lines, 2400 + ot_counter * 300)
    ot_counter <- ot_counter + 1
  }
  date <- format(as.Date(pbp_data$date[1]), "%B %d, %Y")
  
  ### Get in to Appropropriate Format
  x <- rbind(
    dplyr::select(pbp_data, secs_remaining_absolute, win_prob) %>%
      dplyr::mutate(team = "home"),
    dplyr::select(pbp_data, secs_remaining_absolute, win_prob) %>%
      dplyr::mutate("win_prob" = 1 - win_prob,
                    team = "away")
  ) %>%
    dplyr::mutate("secs_elapsed" = max(secs_remaining_absolute) - secs_remaining_absolute)
  
  ### Game Excitemant Index
  pbp_data$wp_delta <- 0
  pbp_data$wp_delta <- abs(pbp_data$win_prob - dplyr::lead(pbp_data$win_prob))
  gei <- sum(pbp_data$wp_delta, na.rm = TRUE) * 2400/msec
  gei <- paste("Game Excitement Index:", round(gei, 2))
  
  ### Minimum Win Probability
  if(pbp_data$score_diff[nrow(pbp_data)] > 0) {
    min_prob <- min(pbp_data$win_prob)
    min_prob <- paste0("Minimum Win Probability for ", home_team, ": ",
                       ifelse(100 * min_prob < 1, "< 1%",
                              paste0(round(100 * min_prob), "%")))
  }else {
    min_prob <- min(1 - pbp_data$win_prob)
    min_prob <- paste0("Minimum Win Probability for ", away_team, ": ",
                       ifelse(100 * min_prob < 1, "< 1%",
                              paste0(round(100 * min_prob), "%")))
  }
  
  ### Make Plot
  p <- ggplot2::ggplot(x, ggplot2::aes(x = secs_elapsed/60, y = win_prob, group = team, col = team)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::theme_minimal() +
    # ggplot2::geom_vline(xintercept = plot_lines/60, lty = 2, alpha = 0.5, size = 0.8) +
    ggplot2::labs(x = "Minutes Elapsed",
                  y = element_blank(),
                  col = element_blank(),
                  title = paste("Win Probability Chart for", home_team, "vs.", away_team),
                  subtitle = date) +
    ggplot2::theme(plot.title = element_text(size = 16),
                   plot.subtitle = element_text(size = 12),
                   axis.title = element_text(size = 14),
                   legend.position = "top", 
                   legend.justification = "left",
                   panel.grid.minor.y = element_blank(), 
                   panel.grid.minor.x = element_blank(),
                   panel.grid.major = element_line(size = .1)) +
    ggplot2::scale_x_continuous(breaks = seq(0, msec/60, 5)) +
    ggplot2::scale_y_continuous(labels = function(x) {paste(100 * x, "%", sep = "")}) +
    ggplot2::scale_color_manual(values = c(away_col, home_col),
                                labels = c(away_team, home_team))
  if(show_gei) {
    p <- p +
      ggplot2::annotate("text", x = 5, y = 0.05, label = gei) +
      ggplot2::annotate("text", x = 5, y = 0.025, label = min_prob)
  }
  
  p
}
