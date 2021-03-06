% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/permp.R
\name{permp}
\alias{permp}
\title{Fuction to compute Pval from given null distribution}
\usage{
permp(testnull, testdata, numperm = 1000)
}
\arguments{
\item{testnull}{Vector of null pval distribution}

\item{testdata}{Vector of pval from dataset of interest}

\item{numperm}{Numeric value of how many permutations were done
Default value 1000}
}
\value{
output Vector of permuted pvalues
}
\description{
Fuction to compute Pval from given null distribution
}
