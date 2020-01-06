############ gg_color_hue helper ###############
#' Get Approiate Model for Time Remaining
#'
#' @export
#' @keywords internal
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
