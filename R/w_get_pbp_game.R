#' Get Play by Play Data
#'
#' @description Scrapes ESPN Play-by-Play data for a single (or multiple) game(s).
#' @usage w_get_pbp_game(game_ids)
#'
#' @param game_ids Vector of ESPN game-IDs
#' @return A data frame of the Play-by-Play data for desired games.
#' @importFrom xml2 read_html
#' @importFrom rvest html_table
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
#' @importFrom magrittr %>% 
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr rename
#' @export

w_get_pbp_game <- function(game_ids) {
  load("R/sysdata.rda")

  if(is.null(game_ids)) {
    stop("game_ids is missing with no default")
  }
  
  # testID <- 401176897
  # otID <- 401179779
  # unplayed <- 401178885
  base_url <- "https://www.espn.com/womens-college-basketball/playbyplay?gameId="
  summary_url <- "https://www.espn.com/womens-college-basketball/game?gameId="
  
  message(paste0("Scraping Data for Game: ", game_ids))
  
  out <- tryCatch({
    url <- paste(base_url, game_ids, sep = "")
    allHTML <- try(xml2::read_html(url))
    tmp <- rvest::html_table(allHTML, fill = TRUE)
    
    if(length(tmp) == 1) {
      message(paste("Play by Play Data Not Available:", game_ids))
    }
    
    gameTableIndex <- grep("PLAY", lapply(tmp, function(x) names(x)))
    
    nPeriod <- 1:length(gameTableIndex)
    
    tmp <- tmp[gameTableIndex]
    
    for(t in nPeriod) {
      tmp[[t]][, "period"] <- t
    }
    
    tmp <- do.call("rbind", tmp)
    
    tmp <- tmp[!is.na(names(tmp))]
    
    pbp <- tmp %>% 
      dplyr::mutate(play_id = 1:nrow(.),
                    time_remaining_period = as.character(time),
                    description = as.character(PLAY),
                    away_score = suppressWarnings(as.numeric(gsub("-.*", "", SCORE))),
                    home_score = suppressWarnings(as.numeric(gsub(".*-", "", SCORE))))
    
    mins <- suppressWarnings(as.numeric(gsub(":.*","", pbp$time_remaining_period)))
    
    secs <- suppressWarnings(as.numeric(gsub(".*:","", pbp$time_remaining_period)))
    
    OTS <- length(unique(pbp$period[pbp$period > 4]))
    
    pbp$secs_remaining <- pmax(10 * (4 - pbp$period), 0) * 60 +
      5 * 60 * pmax((OTS * as.numeric(pbp$period <= 4)), ((OTS + 4 - pbp$period) * as.numeric(pbp$period > 4))) + 
      60 * mins + secs
    
    pbp <- dplyr::select(pbp, play_id, period, time_remaining_period, secs_remaining, description,
                         home_score, away_score)
    
    pbp[1, c("home_score", "away_score")] <- c(0,0)
    
    these <- which(is.na(pbp$home_score))
    
    pbp[these, c("home_score", "away_score")] <- pbp[these - 1 , c("home_score", "away_score")]
    
    ### Get full team names
    url2 <- paste(summary_url, game_ids, sep = "")
    tmp <- xml2::read_html(url2) %>% 
      rvest::html_table(fill = TRUE)
    pbp$away <- as.character(as.data.frame(tmp[[2]])[1,1])
    pbp$home <- as.character(as.data.frame(tmp[[2]])[2,1])
    away_abv <- as.character(as.data.frame(tmp[[1]])[1,1])
    home_abv <- as.character(as.data.frame(tmp[[1]])[2,1])
    
    ### Get Game Line
    # y <- scan(url2, what = "", sep = "\n")
    # y <- y[grep("Line:", y)]
    # if(length(y) > 0) {
    #   y <- gsub("<[^<>]*>", "", y)
    #   y <- gsub("\t", "", y)
    #   y <- strsplit(y, ": ")[[1]][2]
    #   line <- as.numeric(strsplit(y, " ")[[1]][2])
    #   abv <- strsplit(y, " ")[[1]][1]
    #   if(abv == home_abv) {
    #     line <- line * -1
    #   }
    # }else {
    #   line <- NA
    # }
    
    pbp$home_favored_by <- NA
    pbp$game_id <- game_ids
    pbp$date <- allHTML %>% 
      html_nodes("title") %>%
      html_text() %>% 
      regmatches(., regexpr("\\w+\\s[0-9]+,\\s[0-9]{4}", .)) %>% 
      as.Date(., "%B %d, %Y")
    pbp$score_diff <- pbp$home_score - pbp$away_score
    
    ### Win Probability by Play
    pbp$pre_game_prob <- 0.5
    
    
    ### Relative Time
    pbp$secs_remaining_relative <- NA
    msec <- max(pbp$secs_remaining)
    for(k in 1:nrow(pbp)) {
      pbp$secs_remaining_relative[k] <-
        secs_to_model(pbp$secs_remaining[k], msec)[2]
    }
    
    ### Compute Win Prob
    pbp$win_prob <- wp_compute(pbp)
    
    # ### Hardcode to 50-50 if Line = 0 or NA
    # if(is.na(pbp$home_favored_by[1]) | pbp$home_favored_by[1] == 0) {
    #   pbp$win_prob[1] <- 0.5
    # }
    
    ### Time Outs
    timeout <- dplyr::filter(pbp, grepl("Timeout", description)) %>%
      dplyr::filter(description != "Official TV Timeout")
    
    timeout$team <- sapply(timeout$description, function(z) gsub("\\s* Timeout", "", z))
    timeout$tmp <- paste(timeout$team, timeout$secs_remaining)
    timeout <- dplyr::filter(timeout, !duplicated(tmp))
    teams <- unique(timeout$team)
    pos_teams <- c(pbp$home[1], pbp$away[1])
    if(nrow(timeout) > 0) {
      home <- pos_teams[which.min(stringdist::stringdist(teams, pbp$home[1]))]
      away <- setdiff(pos_teams, home)
    }else{
      home <- pos_teams[1]
      away <- pos_teams[2]
    }
    pbp$home_time_out_remaining <- 4
    pbp$away_time_out_remaining <- 4
    pbp$home_timeout_ind <- 0
    pbp$away_timeout_ind <- 0
    nplay <- nrow(pbp)
    if(nrow(timeout) > 0) {
      for(j in 1:nrow(timeout)) {
        play_id <- timeout$play_id[j]
        secs_remaining <- timeout$secs_remaining_relative[j]
        period <- timeout$period[j]
        
        if(timeout$team[j] == home) {
          pbp$home_time_out_remaining[play_id:nplay] <- pbp$home_time_out_remaining[play_id:nplay] - 1
          pbp$home_timeout_ind[pbp$secs_remaining_relative <= secs_remaining & pbp$secs_remaining_relative
                               >= secs_remaining - 60 & pbp$period == period] <- 1
        }else {
          pbp$away_time_out_remaining[play_id:nplay] <- pbp$away_time_out_remaining[play_id:nplay] - 1
          pbp$away_timeout_ind[pbp$secs_remaining_relative <= secs_remaining & pbp$secs_remaining_relative
                               >= secs_remaining - 60 & pbp$period == period] <- 1
        }
      }
    }
    pbp$home_time_out_remaining[pbp$period > 2] <-
      pbp$home_time_out_remaining[pbp$period > 2] + (pbp$period[pbp$period > 2] - 2)
    pbp$away_time_out_remaining[pbp$period > 2] <-
      pbp$away_time_out_remaining[pbp$period > 2] + (pbp$period[pbp$period > 2] - 2)
    
    if(any(pbp$home_time_out_remaining < 0) | any(pbp$away_time_out_remaining < 0)) {
      pbp$home_time_out_remaining <- pbp$home_time_out_remaining + 2
      pbp$away_time_out_remaining <- pbp$away_time_out_remaining + 2
    }else{
      if(max(pbp$home_time_out_remaining[pbp$period == 2]) < 4) {
        pbp$home_time_out_remaining[pbp$period >= 2] <-
          pbp$home_time_out_remaining[pbp$period >= 2] + 1
      }
      if(max(pbp$away_time_out_remaining[pbp$period == 2]) < 4) {
        pbp$away_time_out_remaining[pbp$period >= 2] <-
          pbp$away_time_out_remaining[pbp$period >= 2] + 1
      }
    }
    
    ### Play Length
    pbp$play_length <- 0
    pbp$play_length[1:(nrow(pbp)-1)] <-
      pbp$secs_remaining[1:(nrow(pbp)-1)] -
      pbp$secs_remaining[2:nrow(pbp)]
    
    pbp <- dplyr::select(pbp, -pre_game_prob)
    pbp <- dplyr::select(pbp, play_id, period, time_remaining_period,
                         secs_remaining_relative, secs_remaining, description,
                         home_score, away_score, score_diff, play_length,
                         win_prob, home, away, home_time_out_remaining,
                         away_time_out_remaining, home_timeout_ind,
                         away_timeout_ind, home_favored_by, game_id, date) %>%
      dplyr::rename("secs_remaining_absolute" = secs_remaining,
                    "secs_remaining" = secs_remaining_relative)
    
    if(!exists("pbp_all")) {
      pbp_all <- pbp
    }
    else{
      pbp_all <- rbind(pbp_all, pbp)
    }
    
    if(!exists("pbp_all")) {
      pbp_all <- NULL
    }
    
    return(pbp_all)
  }, 
  error = function(e) {
    return(paste("No data available for", game_ids))
  })
}
