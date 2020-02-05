#' Get Team Roster
#'
#' @description Gets team roster for current season.
#' @usage get_roster(team)
#' 
#' @param team Quoted team name
#' @return A data-frame of the team's roster for current season. Largely internal.
#' @importFrom xml2 read_html
#' @importFrom rvest html_table
#' @export
w_get_roster <- function(team) {
  ### Error Testing
  if(is.null(team)) {
    stop("team is missing with no default")
  }
  if(!team %in% ids$team) {
    stop("Invalid team. Please consult the ids data frame for a list of valid teams, using data(ids).")
  }
  base_url <- "https://www.espn.com/womens-college-basketball/team/roster/_/id/"
  url <-  paste(base_url, ids$id[ids$team == team], "/", ids$link[ids$team == team], sep = "")
  tmp <- try(html_table(read_html(url)))
  if(class(tmp) == "try-error") {
    warning("Unable to get roster. ESPN is updating CBB files. Check back again soon")
    return(NULL)
  }
  tmp <- as.data.frame(tmp[[1]])
  names(tmp) <- c("name", "position", "height", "class", "hometown")
  for(i in 1:ncol(tmp)) {
    tmp[,i] <- as.character(tmp[,i])
  }
  tmp$number <- as.numeric(gsub("[^0-9]", "", tmp$name))
  tmp$name <- gsub("[0-9]*", "", tmp$name)
  tmp <- tmp[order(tmp$number), ]
  return(tmp)
}
