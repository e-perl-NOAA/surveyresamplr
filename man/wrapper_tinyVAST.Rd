% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrapper_tinyVAST.R
\name{wrapper_tinyVAST}
\alias{wrapper_tinyVAST}
\title{Species distribution model function using tinyVAST}
\usage{
wrapper_tinyVAST(x, y, z, dir_spp, spp_info, n_knots = 500)
}
\arguments{
\item{x}{speciesname_df[[i]] which is a data frame from a list of data frames
created from the cleanup_by_species() function and any further post-processing
of depth filters (see the smaller_function.R file for those).}

\item{y}{speciesname_files[[i]] which is an item in a list created from}

\item{z}{A dataframe with the new data to predict on.}

\item{dir_spp}{A character string specifying the directory for output files.}

\item{spp_info}{information about the species test run.}

\item{n_knots}{Numeric. Default  = 500.
names(speciesname_df)}
}
\description{
Function to create a mesh, fit the WHAM model, and get the index.
TOLEDO: need to build out, this is just a holding spot
Exports fit.rds and index.rds files to the designated species folder.
Learn more: https://github.com/vast-lib/tinyVAST
}
\examples{
\dontrun{
wrapper_tinyVAST() # TO DO: NEED EXAMPLE OF HOW TO USE
}
}
