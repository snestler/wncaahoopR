#' Make list of team ID's
#'
#'  NOT SURE YET IF WE NEED THIS OR NOT
#'
#'
### Make ids df (only if package not loaded in memory)
old_create_ids_df <- function() {
  test <- read.csv("https://raw.githubusercontent.com/lbenz730/NCAA_Hoops_Play_By_Play/master/ids.csv",
                   as.is = T)
  teams_url <- "http://www.espn.com/womens-college-basketball/teams"
  x <- scan(teams_url, what = "", sep = "\n")
  x <- x[grep("womens-college-basketball/team/schedule/_/id/", x)][2]
  x <- strsplit(x, "Clubhouse")[[1]]
  
  ids <- data.frame("team" = rep(NA, 351),
                    "id" = rep(NA, 351),
                    "link" = rep(NA, 351)) # Changed from 353 to 351
  
  for(i in 2:length(x)) {
    y <- strsplit(x[i], "womens-college-basketball/team/_/id/")[[1]][2]
    y <- unlist(strsplit(y, "/"))
    ids$id[i-1] <- y[1]
    ids$link[i-1] <- gsub("\".*", "", y[2])
    name <- test$team[ids$link[i-1] == test$link]
    ids$team[i-1] <- ifelse(length(name) > 0, name, NA)
  }
  
  tofill <- which(is.na(ids$team))
  for(i in 1:length(tofill)) {
    k <- which.min(stringdist::stringdist(ids$link[tofill[i]], test$link[tofill]))
    ids$team[tofill[i]] <- test$team[tofill[k]]
  }
  
  return(ids)
}
