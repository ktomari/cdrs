% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_wrappers.R
\name{cdrs_plt_prep}
\alias{cdrs_plt_prep}
\title{Prepare data for plotting.}
\usage{
cdrs_plt_prep(
  data_,
  cols_,
  dict_ = NULL,
  remove_angle_brackets = TRUE,
  is_weighted = TRUE,
  txt_options = NULL,
  sort_ = TRUE,
  title_size = 14,
  param_file = system.file("extdata", "plot_parameters.xlsx", package = "cdrs")
)
}
\arguments{
\item{data_}{the DRS data.}

\item{cols_}{the columns of interest.}

\item{dict_}{the data dictionary. If \code{NULL} no plot label decoration performed. In other words, the plot will not display textual descriptions.}

\item{remove_angle_brackets}{logical. Determines whether to erase angle brackets from \if{html}{\out{<missingness>}} values.}

\item{is_weighted}{logical.}

\item{txt_options}{either NULL or a list providing parameters for \code{cdrs_plt_txt()}.}

\item{sort_}{logical. Sort variables/levels by magnitude of the mean.}

\item{title_size}{numeric. The size of the font for the title. All other fonts scale linearly to title_size, even if a title isn't included.}

\item{param_file}{character. Path to custom parameters xlsx document. See inst/extdata/plot_parameters.xlsx for file structure.}
}
\value{
object of class tibble, data set proportions.
}
\description{
Prepare DRS data for plotting.
}
