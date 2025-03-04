% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_cleaning_functions.R
\name{remove_clicks_counters}
\alias{remove_clicks_counters}
\title{Remove click counter and timer columns}
\usage{
remove_clicks_counters(myData)
}
\arguments{
\item{myData}{A data frame.}
}
\value{
A data frame with no click counters and timers.
}
\description{
This function removes Qualtrics click counters and timers by identifying columns with the following suffixes:
"First.Click", "Last.Click", "Page.Submit", "Click.Count"
}
