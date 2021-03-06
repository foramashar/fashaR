% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addtable2plot.new.R
\name{addtable2plot.new}
\alias{addtable2plot.new}
\title{Updated function to add plot to table}
\usage{
addtable2plot.new(
  x,
  y = NULL,
  table,
  lwd = par("lwd"),
  bty = "n",
  bg = par("bg"),
  cex = 1,
  xjust = 0,
  yjust = 1,
  xpad = 0.1,
  ypad = 0.5,
  box.col = par("fg"),
  text.col = par("fg"),
  col.rowname = par("fg"),
  col.colname = par("fg"),
  display.colnames = TRUE,
  display.rownames = FALSE,
  hlines = FALSE,
  vlines = FALSE,
  title = NULL,
  text.font = NULL
)
}
\arguments{
\item{x}{Numeric vectors for position coord.}

\item{y}{Numeric vectors for position coord.}

\item{table}{Data table to add.}

\item{lwd, bty, bg, cex, xjust, yjust, xpad, ypad}{See par for details.}

\item{box.col, text.col, col.rowname, col.colname}{Details for color of bg and text for different aspects}

\item{display.colnames, display.rownames}{Logical vectors for whether row and column names should be displayed.}

\item{hlines, vlines}{Logical vectors for whether table should have horizontal and vertical lines.}

\item{title}{Value for table header.}

\item{text.font}{See par for details on font.}
}
\description{
Updated function to add plot to table
}
\seealso{
plotrix
}
