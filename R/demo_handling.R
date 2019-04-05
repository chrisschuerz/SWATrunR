#' Load SWAT demo data
#'
#' @param dataset Character string to define data set to load. Valid inputs are
#'   \code{dataset = c('project', 'observation', 'subbasin', 'river', 'hru')}.
#'   \code{dataset = 'project'} loads a SWAT demo project in the defined
#'   \code{path} location and returns the final project path as a text string in
#'   R. The definition of the \code{swat_version} is required.
#'   \code{dataset = 'observation'} returns a \code{tibble} with discharge observation
#'   data at the main outlet of the demo watershed.
#'   The options \code{'subbasin'}, \code{'river'}, and \code{'hru'} return the paths
#'   of the respective subbasin, river network and HRU shape files. See examples
#'   how to load shapes unsing e.g. the \code{sf} package. These options require
#'   a definition of the \code{swat_version}.
#' @param path Character string that defines the path where copy the SWAT demo
#'   project.
#' @param swat_version Character string that defines the SWAT version. Options
#'   are \code{swat_version = c('2012', 'plus')}. This argument is required to
#'   load SWAT projects and shape files.
#'
#' @section Examples:
#'   To learn how to load the different demo data sets with \code{SWATplusR} see the
#'   section \href{https://chrisschuerz.github.io/SWATplusR/articles/SWATplusR.html#loading-demos}{Loading demos}
#'   on the package's *Get Started* page.
#'
#' @importFrom dplyr case_when
#'
#' @export

load_demo <- function(dataset, path = NULL, swat_version = NULL) {
  pkg_path <- system.file(package = "SWATplusR")
  dataset <- tolower(dataset) %>% substr(., 1, 3)
  if(!(dataset %in% c("pro", "obs", "sub", "riv", "hru"))) {
    stop("Invalid selection for dataset!")
  }
  if(dataset != "obs") {
    if(is.null(swat_version)) {
      stop("Loading a dataset = '"%&%dataset%&%"' requires a 'swat_version'.")
    }

    swat_version <- tolower(swat_version)
    if(swat_version == "+")  swat_version <- "plus"
    if(swat_version == 2012) swat_version <- "2012"

    if(!(swat_version %in% c("2012", "plus"))) {
      stop("Invalid value for 'swat_version'. Must be one of: '2012', 'plus', '+'.")
    }
    os <- get_os()
    if(swat_version != "2012" & os == "unix") {
      stop("The package does not yet provide a unix SWAT+ demo.")
    }
  }

  if(dataset == "pro") {
      if(is.null(path)) {
        stop("Loading a SWAT demo requires a 'path'.")
      }
      path <- gsub("\\/$", "", path)
      demo_path <- path%//%"swat"%&%swat_version%_%"demo"

      if(dir.exists(demo_path)) {
        warning("Demo already exists in provided 'path'."%&&%
                "To work with the existing demo project use the returned path.")
        return(demo_path)
      } else {
        add_slash <- ifelse(grepl("\\:$", path), "/", "")
        unzip(zipfile = pkg_path%//%"extdata"%//%swat_version%_%"project"%.%"zip",
              exdir = path%&%add_slash)
        unzip(zipfile = pkg_path%//%"extdata"%//%swat_version%_%os%.%"zip",
              exdir = demo_path)
        return(demo_path)
      }
  }
  if(dataset == "obs") {
      obs <- readRDS(pkg_path%//%"extdata"%//%dataset%.%"rds")
      return(obs)
  }
  if(dataset %in% c("sub", "riv", "hru")) {
      if(is.null(swat_version)) {
        stop("To retrive the shapefile path a 'swat_version' must be provided.")
      }
      return(pkg_path%//%"extdata"%//%swat_version%_%"shapes"%//%dataset%.%"shp")
  }
}
