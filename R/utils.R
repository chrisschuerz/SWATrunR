#' Get the number of digits that the run_XXX should have based on the number
#' of rows in the parameter values table
#'
#' @param tbl Parameter values table
#'
#' @keywords internal
#'
get_digit <- function(tbl) {
  if(is.data.frame(tbl)) {
    n_digit <- tbl %>%
      nrow(.) %>%
      as.character(.) %>%
      nchar(.)
  } else {
    n_digit <- 1
  }
}

#' Get the time interval between to time stamps
#'
#' @param start_time start time stamp
#' @param end_time end time stamp
#'
#' @importFrom lubridate as.period interval
#'
#' @keywords internal
#'
get_time_interval <- function(start_time, end_time) {
  interval(start_time, end_time) %>%
    round(.) %>%
    as.period(.)
}

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

#' Display the progress if iterative processes as percentage value
#'
#' @param n Iteration step
#' @param nmax Number of iterations
#' @param t0 initial time step
#'
#' @importFrom dplyr %>%
#' @importFrom lubridate as.period interval now seconds
#' @keywords internal
#'
display_progress_pct <- function(n, nmax, t0){
  t1 <- now()
  time_elaps  <- interval(t0,t1) %>%
    round(.) %>%
    as.period(.)
  time_remain <- (as.numeric(time_elaps, "seconds")*(nmax-n)/n) %>%
    round(.) %>%
    seconds(.) %>%
    as.period(., unit = "days")
  prog <- paste0(round(100*n/nmax), "%")
  cat("\r", "Progress:", prog,
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
    cat("\r", paste0(rep(" ", 75), collapse = ""))
    interval(t0,now()) %>%
      round(.) %>%
      as.period(.) %>%
      as.character(.) %>%
      cat("\r","Completed",nmax, word%&%plural(nmax), "in", ., "\n")
}

#' Build filter expressions from the parameter constraint table'
#'
#' @param constraints Constraint table
#'
#' @importFrom dplyr %>% mutate select
#' @importFrom purrr map map_chr map2_chr
#' @importFrom tidyselect any_of
#' @keywords internal
#'
build_expression <- function(constraints) {
  constraints %>%
    mutate(file_name = paste0("== '", file_name, "'")) %>%
    select(-par_name, -parameter, - change, - full_name, - any_of('layer')) %>%
    transpose() %>%
    map(., ~.x[!is.na(.)]) %>%
    map(., ~map2_chr(.x, names(.), ~ paste0('filter(., ',.y, .x, ')'))) %>%
    map(., ~ c("table", .x)) %>%
    map_chr(., ~ paste(.x, collapse = " %>% "))
}

#' Evaluate the expression defined for a variable in 'output'
#'
#' @param table Table to which dplyr expression should be applied
#' @param expr Expression to be applied to table
#'
#' @importFrom dplyr %>%
#' @keywords internal
#'
evaluate_expression <- function(table, expr){
    parse(text = expr) %>%
    eval(.)
}

#' Convert character string to numeric without displaying warnings
#'
#' @param chr Character string
#' @keywords internal
#'
as_num <- function(chr) {suppressWarnings(as.numeric(chr))}

#' Add plural 's' to the written message if multiple operations done
#'
#' @param n INterger number of operations
#' @keywords internal
#'
plural <- function(n) {
  ifelse(n == 1, "", "s")
}

#' Concatenate with an underscore
#'
#' \%_\% pastes two strings by "_".
#' @keywords internal
"%_%" <- function(a, b) paste(a, b, sep = "_")

#' Concatenate with a hyphen
#'
#' \%-\% pastes two strings by "-".
#' @keywords internal
'%-%' <- function(a, b) paste(a, b, sep = "-")

#' Concatenate with a hyphen and space
#'
#' \%-\% pastes two strings by " - ".
#' @keywords internal
'%--%' <- function(a, b) paste(a, b, sep = " - ")


#' Concatenate with a dot
#'
#' \%.\% pastes two strings by ".".
#' @keywords internal
'%.%' <- function(a, b) paste(a, b, sep = ".")

#' Paste slash function
#'
#' \%//\% pastes two strings by "/".
#' @keywords internal
'%//%' <- function(a, b) paste(a, b, sep = "/")

#' Concatenate without separator
#'
#' \%&\% pastes two strings by "".
#' @keywords internal
'%&%' <- function(a, b) paste0(a, b)

#' Concatenate with space
#'
#' \%&&\% pastes two strings by " ".
#' @keywords internal
'%&&%' <- function(a, b) paste(a, b, sep = " ")
