seconds_to_display <- function(sec) {
  minutes <- pad(floor(sec / 60))
  seconds <- pad(sec %% 60)
  return(paste0(minutes, ":", seconds))
}

pad <- function(number) {
  if (nchar(number) == 1) {
    return(paste0("0", number))
  }
  return(number)
}

display_to_seconds <- function(disp) {
  time <- strsplit(disp, ":")
  time <- as.numeric(time[[1]])
  out <- time[1] * 60 + time[2]
  return(out)
}
