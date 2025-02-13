library(dplyr)
library(stringr)

strip_non_numbers <- function(vector){
  gsub("[^0-9.-]", "", vector) %>% as.numeric
}

ofRange <- function(x, value = 0.5){
  # quickly find proportions of a vector, useful for plotting!
  # would be nice to have it handling times and other date formats, other than date by day
  stopifnot(is.numeric(x) | lubridate::is.Date(x))
  if(lubridate::is.Date(x)){
    was.Date <- T
    x <- as.numeric(x)
  } else {
    was.Date <- F
  }
  out <- sapply(value, function(v){min(x) + ((max(x) - min(x)) * v)})
  if(was.Date){
    as.Date(out, origin = "1970-01-01")
  } else {
    out
  }
}