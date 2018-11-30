#' Load SWAT demo data
#'
#' @param type Character string that defines the type of demo data set to be
#'   loaded. \code{type = 'SWAT2012'} loads a SWAT2012 project folder.
#'   \code{type = 'SWATplus'} loads a SWAT+ project folder. \code{type =
#'   'obs_data'} returns the observation data for the SWAT demo porject.
#' @param path Character string that defines the path where copy the SWAT demo
#'   project.
#'
#' @importFrom dplyr case_when
#'
#' @export
#'
#'

load_demo <- function(dataset, swat_version = NULL, path = NULL) {
  pkg_path <- system.file(package = "SWATplusR")
  dataset <- tolower(dataset)
  if(!(dataset %in% c("project", "observation", "subbasin", "reach", "hru"))) {
    stop("Invalid selection for dataset!")
  }
  swat_version <- swat_version %>% tolower(.)
  if(swat_version == "+") swat_version <- "plus"

  if(!(swat_version %in% c("2012", "plus"))) {
    stop("Invalid value for 'swat_version'. Must be one of: '2012', 'plus', '+'.")
  }
  os <- get_os()
  if(swat_version != "2012" & os == "unix") {
    stop("The package does not yet provide a unix SWAT+ demo.")
  }

  if(dataset == "project") {
      if(is.null(path)) {
        stop("To retrieve a SWAT demo project a 'path' must be provided.")
      }
      unzip(zipfile = pkg_path%//%"extdata"%//%swat_version%_%"project"%.%"zip",
            exdir = path)
      unzip(zipfile = pkg_path%//%"extdata"%//%swat_version%_%os%.%"zip",
            exdir = path%//%"swat"%&%swat_version%_%"demo")
      return(path%//%type%_%"demo")
  }
  if(dataset == "observation") {
      obs <- readRDS(pkg_path%//%"extdata"%//%dataset%.%"rds")
      return(obs)
  }
  if(dataset %in% c("subbasin", "reach", "outlet", "hru")) {
      if(is.null(swat_version)) {
        stop("To retrive the shapefile path a 'swat_version' must be provided.")
      }
      return(pkg_path%//%"extdata"%//%swat_version%_%"shapes"%//%dataset%.%"shp")
  }
}
