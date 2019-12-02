stripwhite <- function(x) gsub("\\s*$", "", gsub("^\\s*", "", x))

### Function to clean PBP data
clean <- function(data, quarter, OTs) {
  cleaned <- data %>% dplyr::mutate(play_id = 1:nrow(data),
                                    quarter = quarter,
                                    time_remaining_quarter = as.character(V1),
                                    description = as.character(V3),
                                    away_score = suppressWarnings(as.numeric(gsub("-.*", "", V4))),
                                    home_score = suppressWarnings(as.numeric(gsub(".*-", "", V4))))
  cleaned$time_remaining_quarter[1] <- ifelse(quarter <= 4, "20:00", "5:00") # Should 5:00 change or not
  mins <- suppressWarnings(as.numeric(gsub(":.*","", cleaned$time_remaining_quarter)))
  secs <- suppressWarnings(as.numeric(gsub(".*:","", cleaned$time_remaining_quarter)))
  cleaned$secs_remaining <- max(10 * (4 - quarter), 0) * 60 +
    5 * 60 * max((OTs * as.numeric(quarter <= 4)), ((OTs + 4 - quarter) * as.numeric(quarter > 4))) + 60 * mins + secs
  if(quarter == 1) {
    cleaned[1, c("home_score", "away_score")] <- c(0,0)
  }
  cleaned <- select(cleaned, play_id, quarter, time_remaining_quarter, secs_remaining, description,
                    home_score, away_score)
  return(cleaned)
}
