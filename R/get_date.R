### Get Date of Given Game
# Needs to be removed.
get_date <- function(game_id) {
  url <- paste("http://www.espn.com/womens-college-basketball/playbyplay?gameId=", game_id, sep = "")
  y <- scan(url, what = "", sep = "\n")[9]
  y <- unlist(strsplit(y, "-"))
  date <-  stripwhite(y[length(y) - 1])
  date <- as.Date(date, "%B %d, %Y")
  return(date)
}
