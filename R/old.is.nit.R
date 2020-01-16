################################# Checks if Game is in NIT #####################
old.is.nit <- function(game_id) {
  url <- paste("http://www.espn.com/mens-college-basketball/playbyplay?gameId=", game_id, sep = "")
  y <- scan(url, what = "", sep = "\n")
  if(any(grepl("NIT SEASON TIP-OFF", y))) {
    return(F)
  }
  return(sum(grepl("NIT", y)) > 1)
}
