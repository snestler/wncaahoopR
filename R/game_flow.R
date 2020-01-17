#' Game Flow Chart
#'
#' @description Creates a game flow chart with ggplot2.
#' @usage game_flow(pbp_data, home_col, away_col)
#' 
#' @param pbp_data Play-by-play data returned from w_get_pbp_game
#' @param home_col Color of home team for chart. Can be selected but defaults 
#' to primary team color.
#' @param away_col Color of away team for chart. Can be selected but defaults 
#' to primary team color.
#' @details This function takes play-by-play data returned from w_get_pbp_game
#' and returns a ggplot2 object to show the game flow.
#' @return A ggplot2 object
#' @examples
#' pbp_data <- w_get_pbp_game("401176897")
#' game_flow(pbp_data, "red", "black")
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_text
#' @export
game_flow <- function(pbp_data, home_col = NULL, away_col = NULL) {
  ### Error Testing
  if(is.na(pbp_data)) {
    stop("game_id is missing with no default")
  }
  if(is.null(home_col)) {
    home_col <- ncaa_colors$primary_color[ncaa_colors$espn_name == unique(pbp_data$home)]
  }
  if(is.null(away_col)) {
    away_col <- ncaa_colors$primary_color[ncaa_colors$espn_name == unique(pbp_data$away)]
  }

  ### Get Data
  pbp_data
  if(is.null(pbp_data)) {
    warning("PBP Data Not Available for Game Flow Chart")
    return(NULL)
  }
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
    dplyr::select(pbp_data, secs_remaining_absolute, home_score) %>%
      dplyr::mutate("score" = home_score, team = "home") %>%
      dplyr::select(-home_score),
    dplyr::select(pbp_data, secs_remaining_absolute, away_score) %>%
      dplyr::mutate("score" = away_score,
                    "team" = "away") %>%
      dplyr::select(-away_score)
  ) %>%
    dplyr::mutate("secs_elapsed" = max(secs_remaining_absolute) - secs_remaining_absolute)


  ### Message
  avg_sd <- round(sum(pbp_data$play_length * pbp_data$score_diff/max(pbp_data$secs_remaining_absolute)), 2)
  home_win <- pbp_data$home_score[nrow(pbp_data)] > pbp_data$away_score[nrow(pbp_data)]
  avg_sd <- ifelse(home_win, avg_sd, -avg_sd)
  avg_sd <- paste0("Average Score Differential for ",
                  ifelse(home_win, home_team, away_team), ": ", avg_sd)
  max_score <- max(c(pbp_data$home_score, pbp_data$away_score))

  ### Make Plot
  ggplot2::ggplot(x, aes(x = secs_elapsed/60, y = score, group = team, col = team)) +
    ggplot2::geom_step(size = 1) +
    ggplot2::theme_bw() +
    ggplot2::geom_vline(xintercept = plot_lines/60, lty = 2, alpha = 0.5, size = 0.8) +
    ggplot2::labs(x = "Minutes Elapsed",
                  y = "Score",
                  col = "",
                  title = paste("Game Flow Chart for", home_team, "vs.", away_team),
                  subtitle = date) +
    ggplot2::theme(plot.title = element_text(size = 16, hjust = 0.5),
                   plot.subtitle = element_text(size = 12, hjust = 0.5),
                   axis.title = element_text(size = 14),
                   plot.caption = element_text(size = 8, hjust = 0),
                   legend.position = "bottom") +
    ggplot2::scale_x_continuous(breaks = seq(0, msec/60, 5)) +
    ggplot2::scale_color_manual(values = c(away_col, home_col),
                                labels = c(away_team, home_team)) +
    ggplot2::annotate("text", x = 10, y = max_score - 10, label = avg_sd)
}
