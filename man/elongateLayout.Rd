% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/elongateLayout.R
\name{elongateLayout}
\alias{elongateLayout}
\title{Converts a wide 96-well plate layout into a long one}
\usage{
elongateLayout(plate_layout)
}
\arguments{
\item{plate_layout}{A .xlsx spreadsheet mapping samples to wells in a 96 well plate}
}
\value{
A 2-column tibble mapping samples to their original well
}
\description{
Converts a wide 96-well plate layout into a long one
}
