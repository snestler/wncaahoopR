# install.packages("devtools")
devtools::install_github("lbenz730/ncaahoopR")

library(ncaahoopR)

w_get_pbp_game <- function(game_ids) {
  ### Error Testing
  if(all(is.na(game_ids))) {
    stop("game_ids is missing with no default")
  }

  if(!"ncaahoopR" %in% .packages()) {
    ids <- create_ids_df()
  }
  ### Get Play by Play Data
  base_url <- "https://www.espn.com/womens-college-basketball/playbyplay?gameId="
summary_url <- "https://www.espn.com/womens-college-basketball/game?gameId="
j <- 0

for(i in 1:length(game_ids)) {
  message(paste0("Scraping Data for Game: ", i, " of ", length(game_ids)))
  if(is.nit(game_ids[i])) {
    message("NIT Game--Play by Play Data Not Available at this time")
    next
  }
  url <- paste(base_url, game_ids[i], sep = "")
  tmp <- try(XML::readHTMLTable(RCurl::getURL(url)), silent = T)

  for(i in 1:length(game_ids)) {
    message(paste0("Scraping Data for Game: ", i, " of ", length(game_ids)))
    if(is.nit(game_ids[i])) {
      message("NIT Game--Play by Play Data Not Available at this time")
      next
    }
    url <- paste(base_url, game_ids[i], sep = "")
    tmp <- try(XML::readHTMLTable(RCurl::getURL(url)), silent = T)
    ### Check if PBP Data is Available
    if(length(tmp) == 0) {
      message("Play by Play Data Not Available")
      next
    }else if(length(tmp) < ncol(tmp[[1]]) | length(tmp) == 0) {
      message("Play by Play Data Not Available")
      next
    }else{
      t1 <- as.numeric(unlist(strsplit(as.character(tmp[[2]][2,1]), ":")))
      t2 <- as.numeric(unlist(strsplit(as.character(tmp[[2]][5,1]), ":")))
      if(60 * t1[1] + t1[2] < 60 * t2[1] + t2[2]) {
        message("Game In Progress--Play by Play Data Not Available. Please Check Back After the Game")
        next
      }
      j <- j + 1
    }

    n <- length(tmp)
    }
  }

if(ncol(tmp[[1]]) == 6) {
  qtr_1 <- clean(as.data.frame(tmp[[2]]), 1, 0)
  qtr_2 <- clean(as.data.frame(tmp[[3]]), 2, 0)
  qtr_3 <- clean(as.data.frame(tmp[[4]]), 3, 0)
  qtr_4 <- clean(as.data.frame(tmp[[5]]), 4, 0)
  pbp <- rbind(qtr_1, qtr_2, qtr_3, qtr_4)
}

### 1 OT
else if(ncol(tmp[[1]]) == 7 & ((n == 8 & ncol(tmp[[7]]) == 4) | (n == 7 & ncol(tmp[[6]]) == 5))) {
  qtr_1 <- clean(as.data.frame(tmp[[2]]), 1, 0)
  qtr_2 <- clean(as.data.frame(tmp[[3]]), 2, 0)
  qtr_3 <- clean(as.data.frame(tmp[[4]]), 3, 0)
  qtr_4 <- clean(as.data.frame(tmp[[5]]), 4, 0)
  qtr_5 <- clean(as.data.frame(tmp[[6]]), 5, 0)
  pbp <- rbind(qtr_1, qtr_2, qtr_3, qtr_4, qtr_5)
}

### 2 OT
else if(ncol(tmp[[1]]) == 8 & ((n == 9 & ncol(tmp[[7]]) == 4) | (n == 8 & ncol(tmp[[6]]) == 5))) {
  qtr_1 <- clean(as.data.frame(tmp[[2]]), 1, 0)
  qtr_2 <- clean(as.data.frame(tmp[[3]]), 2, 0)
  qtr_3 <- clean(as.data.frame(tmp[[4]]), 3, 0)
  qtr_4 <- clean(as.data.frame(tmp[[5]]), 4, 0)
  qtr_5 <- clean(as.data.frame(tmp[[6]]), 5, 0)
  qtr_6 <- clean(as.data.frame(tmp[[7]]), 6, 0)
  pbp <- rbind(qtr_1, qtr_2, qtr_3, qtr_4, qtr_5, qtr_6)
}

### 3 OT
else if(ncol(tmp[[1]]) == 9 & ((n == 10 & ncol(tmp[[7]]) == 4) | (n == 9 & ncol(tmp[[6]]) == 5))){
  qtr_1 <- clean(as.data.frame(tmp[[2]]), 1, 0)
  qtr_2 <- clean(as.data.frame(tmp[[3]]), 2, 0)
  qtr_3 <- clean(as.data.frame(tmp[[4]]), 3, 0)
  qtr_4 <- clean(as.data.frame(tmp[[5]]), 4, 0)
  qtr_5 <- clean(as.data.frame(tmp[[6]]), 5, 0)
  qtr_6 <- clean(as.data.frame(tmp[[7]]), 6, 0)
  qtr_7 <- clean(as.data.frame(tmp[[8]]), 7, 0)
  pbp <- rbind(qtr_1, qtr_2, qtr_3, qtr_4, qtr_5, qtr_6, qtr_7)
}

### 4 OT
else if(ncol(tmp[[1]]) == 10 & ((n == 11 & ncol(tmp[[7]]) == 4) | (n == 10 & ncol(tmp[[6]]) == 5))) {
  qtr_1 <- clean(as.data.frame(tmp[[2]]), 1, 0)
  qtr_2 <- clean(as.data.frame(tmp[[3]]), 2, 0)
  qtr_3 <- clean(as.data.frame(tmp[[4]]), 3, 0)
  qtr_4 <- clean(as.data.frame(tmp[[5]]), 4, 0)
  qtr_5 <- clean(as.data.frame(tmp[[6]]), 5, 0)
  qtr_6 <- clean(as.data.frame(tmp[[7]]), 6, 0)
  qtr_7 <- clean(as.data.frame(tmp[[8]]), 7, 0)
  qtr_8 <- clean(as.data.frame(tmp[[9]]), 8, 0)
  pbp <- rbind(qtr_1, qtr_2, qtr_3, qtr_4, qtr_5, qtr_6, qtr_7, qtr_8)
}

these <- grep(T, is.na(pbp$home_score))
pbp[these, c("home_score", "away_score")] <- pbp[these - 1 , c("home_score", "away_score")]

### Get full team names
url2 <- paste(summary_url, game_ids[i], sep = "")
tmp <- XML::readHTMLTable(RCurl::getURL(url2))
pbp$away <- as.character(as.data.frame(tmp[[2]])[1,1])
pbp$home <- as.character(as.data.frame(tmp[[2]])[2,1])
away_abv <- as.character(as.data.frame(tmp[[1]])[1,1])
home_abv <- as.character(as.data.frame(tmp[[1]])[2,1])

### Get Game Line
y <- scan(url2, what = "", sep = "\n")
y <- y[grep("Line:", y)]
if(length(y) > 0) {
  y <- gsub("<[^<>]*>", "", y)
  y <- gsub("\t", "", y)
  y <- strsplit(y, ": ")[[1]][2]
  line <- as.numeric(strsplit(y, " ")[[1]][2])
  abv <- strsplit(y, " ")[[1]][1]
  if(abv == home_abv) {
    line <- line * -1
  }
}else {
  line <- NA
}

pbp$home_favored_by <- line
pbp$play_id <- 1:nrow(pbp)
pbp$game_id <- game_ids[i]
pbp$date <- get_date(game_ids[i])
pbp$score_diff <- pbp$home_score - pbp$away_score

return(pbp)
}

