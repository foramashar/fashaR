% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forestplot.boxcol.R
\name{forestplot.boxcol}
\alias{forestplot.boxcol}
\title{Modified forestplot function to allow for different colored
boxes}
\usage{
forestplot.boxcol(
  labeltext,
  mean,
  lower,
  upper,
  align = NULL,
  is.summary = FALSE,
  clip = c(-Inf, Inf),
  xlab = "",
  zero = 0,
  graphwidth = unit(2, "inches"),
  col = meta.colors(),
  xlog = FALSE,
  xticks = NULL,
  boxsize = NULL,
  ...
)
}
\value{
Graphical output
}
\description{
Modified forestplot function to allow for different colored
boxes
}
\examples{
forestplot.new(as.matrix(blah), mean=test.or, lower=test.l, upper=test.u, zero=1, 
is.summary=c(rep(c(T, rep(F,4)),3),T),
col=meta.colors(box=c(NA, rep("royalblue",4),NA,rep("red",4),NA, rep("purple3",4)),lines=c("grey","red"), summary="black"))
}
\seealso{
rmeta, forestplot
}
