######################### Get Game IDs ########################################
#' Get Team game_ids
#'
#' Gets team game_ids for current season.
#'
#' @param team Team to get game_ids
#' @return A vector of the team's ESPN game_ids for current season
#' @importFrom rvest html_attr
#' @importFrom rvest html_elements
#' @importFrom xml2 read_html
#' @export
w_get_game_ids <- function(team) {
  ### Error Testing
  if(is.null(team)) {
    stop("team is missing with no default")
  }
  base_url <- "http://www.espn.com/womens-college-basketball/team/_/id/"
  url <- paste(base_url, ids$id[ids$team == team], "/", ids$link[ids$team == team], sep = "")
  x <- html_attr(html_elements(read_html(url), ".club-schedule a[href*='gameId'][name*='clubhouse']"), 
                 "href")
  x <- regmatches(x, regexpr("[0-9]+$", x))
  return(x)
}
