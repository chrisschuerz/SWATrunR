#' Loading SWAT demo data
#'
#' With this function you can load data sets for SWAT2012 and SWAT+ demo projects.
#' The provided data includes demo projects, discharge observation data and spatial
#' data such as the subbasin boundaries, the river networks, and the HRUs of the
#' SWAT setups.
#'
#' @param dataset Character string to define data set to load. Valid inputs are
#'   \code{dataset = c('project', 'observation', 'subbasin', 'river', 'hru')}.
#'   \code{dataset = 'project'} loads a SWAT demo project in the defined
#'   \code{path} location and returns the final project path as a text string in
#'   R. The definition of the \code{version} is required.
#'   \code{dataset = 'observation'} returns a \code{tibble} with discharge observation
#'   data at the main outlet of the demo watershed.
#'   The options \code{'subbasin'}, \code{'river'}, and \code{'hru'} return the paths
#'   of the respective subbasin, river network and HRU shape files. See examples
#'   how to load shapes unsing e.g. the \code{sf} package. These options require
#'   a definition of the \code{version}.
#' @param path Character string that defines the path where copy the SWAT demo
#'   project.
#' @param version Character string that defines the SWAT version. Options
#'   are \code{version = c('2012', 'plus')}. This argument is required to
#'   load SWAT projects and shape files.
#' @param revision Numeric value to define the model revision ofthe loaded SWAT
#'   project. See \code{\link[SWATdata]{SWATdata}} for valid revision numbers.
#'   When loading a SWAT project and leaving \code{revision = NULL}, then the
#'   most recent model revision is loaded by default.
#'
#' @section Examples:
#'   To learn how to load the different demo data sets with \code{SWATplusR} see the
#'   section \href{https://chrisschuerz.github.io/SWATplusR/articles/SWATplusR.html#loading-demos}{Loading demos}
#'   on the package's *Get Started* page.
#'
#' @importFrom stringr str_sub
#'
#' @export

load_demo <- function(dataset, path = NULL, version = NULL, revision = NULL) {

  if(!("SWATdata" %in% installed.packages()[,1])) {
    choice <- select.list(choices = c("install", "cancel"),
      title = paste("Loading demo data requires the R package 'SWATdata'.",
                    "Should the package now be installed?",
                    "Type '1' to install or '2' to cancel:"))
    if(choice == "install") {
      if(!("devtools" %in% installed.packages()[,1])){
        install.packages("devtools")
      }
      suppressWarnings(devtools::install_github("chrisschuerz/SWATdata"))
    }
  }
  pkg_path <- system.file(package = "SWATdata")

  dataset <- tolower(dataset) %>% str_sub(., 1, 3)
  if(!(dataset %in% c("pro", "obs", "sub", "riv", "hru"))) {
    stop("Invalid selection for dataset!")
  }
  if(dataset != "obs") {
    if(is.null(version)) {
      stop("Loading a dataset = '"%&%dataset%&%"' requires a SWAT 'version'.")
    }

    version <- tolower(version)
    if(version == "+")  version <- "plus"
    if(version == 2012) version <- "2012"

    if(!(version %in% c("2012", "plus"))) {
      stop("Invalid value for 'version'. Must be one of: '2012', 'plus', '+'.")
    }
  }

  if(dataset == "pro") {

    os <- get_os()
    demo_files <- list.files(pkg_path%//%"extdata")

    if(is.null(revision)) {
      revision <- demo_files %>%
        .[grepl(version, .)] %>%
        .[grepl(os, .)] %>%
        str_sub(., 6, nchar(.)) %>%
        gsub("[^[:digit:]]", "",.) %>%
        as.numeric(.) %>%
        max(.)
    }

    swat_exe <- version%_%"rev"%&%revision%_%os%.%"zip"
    swat_project <- version%_%"rev"%&%revision%_%"project"%.%"zip"

    if(!(swat_exe %in% demo_files)) {
      stop("There is no SWAT"%&%version%&&%"Rev."%&%revision%&&%
           "project available for '"%&%os%&%"'.")
    }

    if(is.null(path)) {
      stop("Loading a SWAT demo project requires a 'path'.")
    }
    path <- gsub("\\/$", "", path)
    demo_path <- path%//%"swat"%&%version%_%"rev"%&%revision%_%"demo"



    if(dir.exists(demo_path)) {
      warning("Demo already exists in provided 'path'."%&&%
              "To work with the existing demo project use the returned path.")
      return(demo_path)
    } else {
      add_slash <- ifelse(grepl("\\:$", path), "/", "")
      unzip(zipfile = pkg_path%//%"extdata"%//%swat_project,
            exdir = path%&%add_slash)
      unzip(zipfile = pkg_path%//%"extdata"%//%swat_exe,
            exdir = demo_path)
      if(os == "unix"){
        system("chmod -R 777"%&&%demo_path%//%"swat"%&%version%_%"rev"%&%revision)
      }
      return(demo_path)
    }
  }

  if(dataset == "obs") {
      obs <- readRDS(pkg_path%//%"extdata"%//%dataset%.%"rds")
      return(obs)
  }

  if(dataset %in% c("sub", "riv", "hru")) {
    return(pkg_path%//%"extdata"%//%version%_%"shapes"%//%dataset%.%"shp")
  }
}
