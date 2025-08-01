% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rank_decomposition_exact.R
\name{rank_decomposition_exact}
\alias{rank_decomposition_exact}
\title{Exact decomposition algorithm for rank aggregations}
\usage{
rank_decomposition_exact(input_data, Lambda, alpha = NaN, show_bar = FALSE)
}
\arguments{
\item{input_data}{Data frame containing 3 columns of ranks/ratings per item.
Each row represents one item.}

\item{Lambda}{A data frame where each row represents one grid point in the weight set.}

\item{alpha}{The value for alpha if computing top-alpha regions}

\item{show_bar}{Boolean for a progress bar and time data printed in console}
}
\value{
An extended data frame with column for TopAlpha, which are factors (of strings) that list
the top ranked items (ordered numerically not by rank)
}
\description{
Compute data frames which represent the exact decomposition of weight set
into all indifference regions representing all possible weighted rank aggregations.
The process requires multiple steps, including:
\enumerate{
\item computing line segments that bisect the weight set
\item computing intersections among these line segments (and the boundaries of weight set)
\item labeling all valid ranks for these intersections
\item computing the convex hull of every aggregate rank's indifference region.
All four data frames will be returned.
}
}
\examples{
Lambda <- weight_set(stepsize=0.1)
rankdf <- data.frame('rank1'=c(1,2,3,4,5), 
           'rank2'=c(2,3,1,5,4), 'rank3'=c(3,1,5,4,2))
Lambda <- rank_aggregation_grid(3,Lambda,rankdf)
inputlist <- rank_decomposition_exact(rankdf,Lambda)
Lambda <- rank_topalpha_grid(Lambda,alpha=3,rankdf)
inputlist <- rank_decomposition_exact(rankdf,Lambda,alpha=3,show_bar=TRUE)
}
