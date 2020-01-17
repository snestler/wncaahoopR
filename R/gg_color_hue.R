#' ggplot Color Hue
#' 
#' @description Gets the hues for circle_assist_net given the number of players.
#' @usage gg_color_hue(n)
#' 
#' @export
#' @keywords internal
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
