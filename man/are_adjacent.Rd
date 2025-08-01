% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/are_adjacent.R
\name{are_adjacent}
\alias{are_adjacent}
\title{Determine if two rankings are adjacent in the decomposition}
\usage{
are_adjacent(num_metrics = 3, label1, label2, metrics, test = "basic")
}
\arguments{
\item{num_metrics}{The dimension of the problem indicating total number of metrics to be aggregated
(3, 4, or 5)}

\item{label1}{String for the first ranked list}

\item{label2}{String for the second ranked list}

\item{metrics}{Data frame for 3 metrics, one metric per column, one row per item, smaller values are better.}

\item{test}{String to label the type of test to check: 'basic' or...}
}
\value{
TRUE or FALSE based on tests
}
\description{
Uses tests for adjacency to determine if rank aggregations have adjacent indifference regions.
The 'basic' test uses Rank.Label and returns true if all pairs of transpositions are adjacent and degenerate.
}
\examples{
metrics <- data.frame('risk1'=c(1,2,3,4,5), 
           'risk2'=c(2,3,1,5,4), 'risk3'=c(3,1,5,4,2))
are_adjacent(num_metrics=3,'1.2.3.4.5', '1.3.2.4.5', metrics)
are_adjacent(num_metrics=3,'1.2.3.4.5', '1.3.2.5.4', metrics)
metrics <- data.frame('risk1'=c(1,2,3,4,5), 
           'risk2'=c(1,2,3,4,5), 'risk3'=c(1,3,2,5,4))
are_adjacent(num_metrics=3,'1.2.3.4.5', '1.3.2.5.4', metrics)
}
