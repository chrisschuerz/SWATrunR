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

#' Concatenate with or
#'
#' \%|\% pastes two strings with a "|".
#' @keywords internal
'%|%' <- function(a, b) paste(a, b, sep = "|")


