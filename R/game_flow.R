#' Game Flow Chart
#'
#' @description Creates a game flow chart with ggplot2.
#' @usage game_flow(.data, home_col, away_col)
#' 
#' @param .data Play-by-play data returned from w_get_pbp_game
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
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @import ggplot2
#' @import grid
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 geom_step
#' @importFrom ggplot2 geom_vline
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_minimal
#' @importFrom grid grid.draw
#' @importFrom grid textGrob
#' @export
game_flow <- function(.data, home_col = NULL, away_col = NULL) {

  pbp_data <- .data
  ### Error Testing
  if(is.null(pbp_data)) {
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
    select(pbp_data, secs_remaining_absolute, home_score) %>%
      mutate("score" = home_score, team = "home") %>%
      select(-home_score),
    select(pbp_data, secs_remaining_absolute, away_score) %>%
      mutate("score" = away_score,
                    "team" = "away") %>%
      select(-away_score)
  ) %>%
    mutate("secs_elapsed" = max(secs_remaining_absolute) - secs_remaining_absolute)


  ### Message
  avg_sd <- round(sum(pbp_data$play_length * pbp_data$score_diff/max(pbp_data$secs_remaining_absolute)), 2)
  home_win <- pbp_data$home_score[nrow(pbp_data)] > pbp_data$away_score[nrow(pbp_data)]
  avg_sd <- ifelse(home_win, avg_sd, -avg_sd)
  avg_sd <- paste0("Average Score Differential for ",
                  ifelse(home_win, home_team, away_team), ": ", avg_sd)
  max_score <- max(c(pbp_data$home_score, pbp_data$away_score))

  ### Make Plot
  t1 <- textGrob(expr("Game Flow for " * phantom(!!home_team) * " vs " * phantom(!!away_team)),
                       just = "top", x = .25, y = 1.1, gp = gpar(col = "black", fontsize = 16))
  
  t2 <- textGrob(expr(phantom("Game Flow for ") * !!home_team * phantom(" vs ") * phantom(!!away_team)),
                       just = "top", x = .25, y = 1.1, gp = gpar(col = home_col, fontsize = 16))
  
  t3 <- textGrob(expr(phantom("Game Flow for ") * phantom(!!home_team) * phantom(" vs ") * !!away_team),
                       just = "top", x = .25, y = 1.1, gp = gpar(col = away_col, fontsize = 16))
  
  p <- ggplot(x, aes(x = secs_elapsed/60, y = score, group = team, color = team)) +
    geom_step(size = 1) +
    theme_minimal() +
    labs(x = "Minutes Elapsed",
                  y = "Score",
                  col = element_blank(),
                  title = "",
                  subtitle = paste(date, avg_sd, sep = "\n")) +
    theme(plot.title = element_text(size = 16),
                   plot.subtitle = element_text(size = 12),
                   axis.title = element_text(size = 14),
                   legend.position = "none", 
                   legend.justification = "left",
                   panel.grid.minor.y = element_blank(), 
                   panel.grid.minor.x = element_blank(),
                   panel.grid.major = element_line(size = .1))+
    scale_x_continuous(breaks = seq(0, msec/60, 5)) +
    scale_color_manual(values = c(away_col, home_col),
                                labels = c(away_team, home_team)) 
  
  g <- ggplot_gtable(ggplot_build(p))
  
  g$grobs[[16]] <- grobTree(t1, t2, t3)
  
  g$layout$clip[g$layout$name == "panel"] <- "off"
  
  plot.new()
  grid.draw(g, recording = FALSE)
}
