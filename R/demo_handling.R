#' Load SWAT demo data
#'
#' @param type Character string that defines the type of demo data set to be
#'   loaded. \code{type = 'SWAT2012'} loads a SWAT2012 project folder.
#'   \code{type = 'SWATplus'} loads a SWAT+ project folder. \code{type =
#'   'obs_data'} returns the observation data for the SWAT demo porject.
#' @param path Character string that defines the path where copy the SWAT demo
#'   project.
#'
#' @export
#'
#'

load_demo <- function(type, path = NULL) {
  pkg_path <- system.file(package = "SWATplusR")
  type <- tolower(type)
  os <- get_os()
  if(type == "swatplus" & os == "unix") {
    stop("The package does not yet provide a unix SWAT+ demo.")
  }

  if(!(type %in%c("swat2012", "swatplus", "obs_data"))) {
    stop("'type' must be either 'swat2012', 'swatplus', or 'obs_data'.")
  }

  if(type %in% c("swat2012", "swatplus") & is.null(path)) {
    stop("To retrieve a SWAT demo project a 'path' must be provided.")
  } else if (type %in% c("swat2012", "swatplus")) {
    unzip(zipfile = pkg_path%//%"extdata"%//%type%_%"demo"%.%"zip",
          exdir = path)
    unzip(zipfile = pkg_path%//%"extdata"%//%type%_%os%.%"zip",
          exdir = path%//%type%_%"demo")
    return(path%//%type%_%"demo")
  }

  if(type == "obs_data") {
    readRDS(pkg_path%//%"extdata"%//%"obs_data.rds")
    return(obs_data)
  }
}
