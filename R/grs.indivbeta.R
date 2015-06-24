#' Function to get adjusted betas from package gtx 
#'
#' Function to get betas for individual SNPs used to calculate
#' ahat from function grs.summary in package gtx
#' @param w Coefficients for the risk score.
#' @param b Aligned beta coefficients in the testing dataset, of same 
#' length as w.
#' @param s Standard errors for â, of same length as  w and b. 
#' @param outputname Optional argument to name of columns of output
#' dataset. If NULL (default), output matrix has column names ahat
#' and aSE.
#'
#' @export
#'
#' @seealso gtx, grs.plot

grs.indivbeta=function (w, b, s, outputname=NULL) 
{
    stopifnot(length(b) == length(w))
    stopifnot(length(s) == length(w))
    f <- !is.na(w) & !is.na(b) & !is.na(s)
    m <- sum(f)
    ahat <- (w[f] * b[f] * s[f]^-2)/(w[f]^2 * s[f]^-2)
    aSE <- sqrt(1/(w[f]^2 * s[f]^-2))
    output=cbind(ahat,aSE)
	if (!is.null(outputname)){
		colnames(output)=outputname
	}
	return(output)
}

