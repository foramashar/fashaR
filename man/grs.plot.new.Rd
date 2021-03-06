% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grs.plot.new.R
\name{grs.plot.new}
\alias{grs.plot.new}
\title{Updated grs.plot function from package gtx}
\usage{
grs.plot.new(
  w,
  b,
  s,
  af,
  col.by = NULL,
  col = NULL,
  text = NULL,
  textpos = NULL,
  textcex = 0.5,
  alpha = 0.05,
  display.legend = FALSE,
  display.pvaldir = FALSE
)
}
\arguments{
\item{col.by}{name of column in dataset to color by (for eg: af)}

\item{display.legend}{logical input}
}
\description{
Modified function form of the function grs.plot from package gtx
that allows coloring by allele frequency.
}
\examples{
with(test, grs.plot.new(get(paste("Effect",trait, sep=".")), AdjustedSCD.Effect, StdErr,Freq1, col.by="af"))
}
\seealso{
gtx, grs.plot
}
