#' Seconds to Model Helper
#' 
#' @description Get appropriate model for time remaining.
#' @usage secs_to_model(sec, msec)
#'
#' @return list of minutes and seconds remaining
#' @export
#' @keywords internal

secs_to_model <- function(sec, msec) {
  offset <- msec - 2400
  if(offset == 300 & sec > offset) {
    sec <- sec - offset
  }
  if(offset == 600) {
    if(sec > 600) {
      sec <- sec - offset
    }
    else if (sec < 600 & sec > 300) {
      sec <- sec - 300
    }
  }
  else if(offset == 900) {
    if(sec > 900) {
      sec <- sec - offset
    }
    else if (sec <= 900 & sec > 600) {
      sec <- sec - 600
    }
    else if (sec <= 600 & sec > 300) {
      sec <- sec - 300
    }
  }
  else if(offset == 1200) {
    if(sec > 1200) {
      sec <- sec - offset
    }
    else if (sec <= 1200 & sec > 900) {
      sec <- sec - 900
    }
    else if (sec <= 900 & sec > 600) {
      sec <- sec - 600
    }
    else if (sec <= 600 & sec > 300) {
      sec <- sec - 300
    }
  }
  
  if(sec == 0) {
    m <- 1
  }
  else if(sec >= 1 & sec < 5) {
    m <- 5
  }
  else if(sec >= 5 & sec <= 10) {
    m <- 10
  }
  else if(sec > 10 & sec <= 30) {
    m <- sec + 1
  }
  else if(sec > 30 & sec <= 60) {
    m <- 31 + floor((sec - 30)/2)
  }
  else if(sec > 60 & sec < 2400) {
    m <- 46 + floor((sec - 60)/10)
  }
  else{
    m <- 279
  }
  return(c(m, sec))
}
