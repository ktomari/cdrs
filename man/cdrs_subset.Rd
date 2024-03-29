% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_wrangling.R
\name{cdrs_subset}
\alias{cdrs_subset}
\title{Prepare and subset DRS data for use with cdrs_design().}
\usage{
cdrs_subset(data_, cols_)
}
\arguments{
\item{data_}{is the complete DRS dataset.}

\item{cols_}{a character vector of column names.}
}
\description{
Subsets data and removes missing values.
}
\examples{
df <- data.frame(
  Q1 = c("Y", NA, "N"),
  Zone = c(1, 2, 3),
  WTFINAL = c(.8, 1, 1.2)
)
drs_subset <- cdrs_subset(data_ = df, "Q1")
}
