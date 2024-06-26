% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_plot_wrappers.R
\name{zoom_sf}
\alias{zoom_sf}
\title{Sets zoom level for ggplot map.}
\usage{
zoom_sf(plot_obj, boundary_obj, dist, zoom = T)
}
\arguments{
\item{plot_obj}{is a ggplot2 object.}

\item{boundary_obj}{is the object around which to build a box.}

\item{dist}{is the distance in map units to buffer around the \code{boundary_obj}. If the crs is EPSG 3310, the units are in meters.}

\item{zoom}{logical. Whether to return the bbox early.}
}
\description{
Sets zoom level for ggplot map.
}
\details{
Add this function to the end of your
ggplot definition.
}
