############ logit helper ###############
#' Define Logit function
#'
#' @export
#' @keywords internal

logit <- function(x) {
  tmp <- exp(x)
  case_when(
    tmp == Inf ~ 1,
    tmp == -Inf ~ 0,
    T ~ tmp/(1 + tmp)
  )
}
