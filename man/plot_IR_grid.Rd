% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_IR_grid.R
\name{plot_IR_grid}
\alias{plot_IR_grid}
\title{Plot one indifference region for the grid search}
\usage{
plot_IR_grid(
  Lambda,
  weight = 0,
  rank.label = "",
  item.label = "",
  triangle = "equilateral",
  bias_axes = TRUE,
  annotations = TRUE,
  plotly_text = FALSE,
  leg.pos = "none"
)
}
\arguments{
\item{Lambda}{Data frame containing weight vectors and labels for weighted rank aggregations.
Must include column names lambda1/2 for right triangle representation or
equilambda1/2 for equilateral triangle representation.
Must include column name Label for the string/factor identifier of the rank aggregation.}

\item{weight}{Weight vector identifying the indifference region to plot.}

\item{rank.label}{The string identifying the indifference region to plot.
This should be a string with the indices separated by periods.}

\item{item.label}{The order of items identifying the indifference region to plot.
This should be a string with the indices separated by periods.}

\item{triangle}{Specify whether the right triangle representation ("right") or the
equilateral triangle representation ("equilateral"). Note that the equilateral triangle
representation is recommended as it is unbiased, and the right triangle representation is
biased.}

\item{bias_axes}{Boolean for whether to include bias axes in the graph}

\item{annotations}{Boolean for whether to include annotations in the graph}

\item{plotly_text}{Boolean for whether to include text which is useful for plotly conversion.
Note that a warning will occur about an "unknown aesthetics" (text); this is a
feature which is intended to be used by the ggplotly interactive library. The warning is not
fixable, and should be ignored. For ggplotly visual tool, use
ggplotly(g,tooltip = 'text') to access these aesthetics as appropriate labels.}

\item{leg.pos}{Input for ggplot legend positioning, e.g. 'bottom', 'right', or c(0.8,0.8)}
}
\value{
A ggplot structure which can be plotted directly.
}
\description{
Input either known ranked order (as a string, separated by periods) or a weight vector.
The indifference region for the given ranked order (or containing the weight vector)
will be plotted in the weight set.
}
\examples{
Lambda <- weight_set()
metrics <- data.frame('cost'=c(10,20,30,40), 'time'=c(5.9, 3.3, 2.5, 4.1), 'risk'=c(1,4,3,2))
Lambda <- rank_aggregation_grid(3,Lambda,metrics)
plot_IR_grid(Lambda,weight=c(0.2,0.2,0.6))
plot_IR_grid(Lambda,rank.label='1.2.3.4')
g <- plot_IR_grid(Lambda,item.label='1.3.2.4')
g
plot_IR_grid(Lambda,weight=c(0.2,0.2,0.6),triangle='right',bias_axes=FALSE,annotations=FALSE)
g <- plot_IR_grid(Lambda,weight=c(0.2,0.2,0.6),plotly_text = TRUE)
plotly::ggplotly(g, tooltip='text')
}
