#' Logit helper function
#' 
#' @description defines the logit function
#' @usage logit(x)
#'
#' @export
#' @importFrom dplyr case_when
#' @keywords internal

logit <- function(x) {
  tmp <- exp(x)
  case_when(
    tmp == Inf ~ 1,
    tmp == -Inf ~ 0,
    T ~ tmp/(1 + tmp)
  )
}
