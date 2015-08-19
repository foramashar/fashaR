#' Fuction to compute Pval from given null distribution
#'
#' @param testnull Vector of null pval distribution
#' @param testdata Vector of pval from dataset of interest
#' @export
#'
#' @return output Vector of permuted pvalues
#'

permp=function(testnull, testdata){
	test=sapply(testdata, function(x){
		if (x<min(testnull)){
			return("<0.01")
		} else {
			return(as.character(table(x<testnull)["FALSE"]/length(na.omit(testnull))))
		}
	})
	return(test)
}

