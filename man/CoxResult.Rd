% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CoxResult.R
\name{CoxResult}
\alias{CoxResult}
\title{Function to tidy up results from CoxPH model}
\usage{
CoxResult(
  coxph.model,
  rowname.suffix,
  colname.suffix = NULL,
  confint = FALSE,
  prettyDig = NULL
)
}
\arguments{
\item{coxph.model}{An object of class coxph from package survival}

\item{rowname.suffix}{Suffix to add to rownames to resulting table}

\item{colname.suffix}{Optional argument to add suffix to column names of resulting table}

\item{confint}{Logical argument to determine whether confindence intervals need to be included in the output}

\item{prettyDig}{Optional numeric input to indicate number of significant digits in the output table}
}
\value{
output Dataframe with summary of results form a coxph object
}
\description{
Function to tidy up results from CoxPH model
}
\examples{
test.coxph=coxph(Surv(DNA.visit.FUTIME.DTH, DEAD12) ~ GENDER+resid.mtDNA+DNA.visit.AGE+CENTER, data=test)
test.results=CoxResult(test.coxph, "all", prettyDig=3)
}
