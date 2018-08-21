% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_model_functions.R
\name{build_model_run}
\alias{build_model_run}
\title{Generate folder structure for parallel SWAT execution}
\usage{
build_model_run(project_path, run_path, n_thread, abs_swat_val, quiet)
}
\arguments{
\item{project_path}{Path to the SWAT project folder (i.e. TxtInOut)}

\item{n_thread}{Number of parallel threads that will be created. This number
must be in accordance to the number of cores of the PC}

\item{abs_swat_val}{Replace internal Absolute_SWAT_Values.txt' file with
custom one found in this path if provided}

\item{project_path}{Path where the '.model_run' folder is built. If NULL the
executable model is built in the 'project_path'}

\item{file_cio}{The file_cio from the 'project_folder' modified according to
'start_date', 'end_date', 'output_interval', and 'years_skip'}
}
\description{
Generate folder structure for parallel SWAT execution
}
\keyword{internal}
