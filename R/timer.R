# I'm using an R6 class for this project because the timer has state.
# this may be a bit overkill for this particular problem, but you can imagine
# some additional use cases like pausing, adding time, or resetting the timer
# where this would be useful.
#
# I also decided to store the time in seconds, and push all of the "00:12" stuff
# to the display side. This results in a bit more converting back and forth than
# is necessary but makes sure that the time is always consistent for the object.
timer <- R6::R6Class(
  "timer",
  public = list(
    seconds = 0,
    start_time = NULL,
    print = function() {
      cat(seconds_to_display(self$seconds))
    },
    enter_time = function(key_press){
      time <- add_time(key_press, seconds_to_display(self$seconds))
      self$seconds <- display_to_seconds(time)
      print(time)
    },
    start = function() {
      self$start_time <- Sys.time()
      while (calc_diff_time(Sys.time(), self$start_time) < self$seconds) {
        time_left <- self$seconds - calc_diff_time(Sys.time(), self$start_time)
        print(seconds_to_display(time_left))
        Sys.sleep(1)
      }
      print("Done!")
    }
  ))

calc_diff_time <- function(t1, t2) {
  out <- as.numeric(difftime(t1, t2, units = "s"))
  return(floor(out))
}

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

add_time <- function(entry, disp = "00:00") {
  if (substring(disp, 1, 1) != "0") {
    return(disp)
  }
  numbers <- gsub( ":", "", disp)
  numbers <- paste0(substring(numbers, 2, 5), entry)
  out <- paste0(substring(numbers, 1, 2), ":", substring(numbers, 3, 4))
  return(out)
}



