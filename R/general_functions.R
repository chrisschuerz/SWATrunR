#' Display the progress if iterative processes
#'
#' @param n Iteration step
#' @param nmax Number of iterations
#' @param t0 initial time step
#'
#' @importFrom dplyr %>%
#' @importFrom lubridate as.period interval now seconds
#' @keywords internal
#'
display_progress <- function(n, nmax, t0, word){
  t1 <- now()
  time_elaps  <- interval(t0,t1) %>%
    round(.) %>%
    as.period(.)
  time_remain <- (as.numeric(time_elaps, "seconds")*(nmax-n)/n) %>%
    round(.) %>%
    seconds(.) %>%
    as.period(., unit = "days")

  cat("\r", word, n, "of", nmax,
      "  Time elapsed:", as.character(time_elaps),
      "  Time remaining:", as.character(time_remain),
      "   ")
}

#' Print message for completed process
#'
#' @param nmax Number of iterations
#' @param t0 initial time step
#'
#' @importFrom dplyr %>%
#' @importFrom lubridate as.period interval now
#' @keywords internal
#'
finish_progress <- function(nmax, t0, word) {
    plural <- ifelse(nmax == 1, "", "s")
    cat("\r", paste0(rep(" ", 75), collapse = ""))
    interval(t0,now()) %>%
      round(.) %>%
      as.period(.) %>%
      as.character(.) %>%
      cat("\r","Completed",nmax, word%&%plural, "in", ., "\n")
}
