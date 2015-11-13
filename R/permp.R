#' Fuction to compute Pval from given null distribution
#'
#' @param testnull Vector of null pval distribution
#' @param testdata Vector of pval from dataset of interest
#' @param numperm Numeric value of how many permutations were done
#'        Default value 1000
#' @export
#'
#' @return output Vector of permuted pvalues
#'

permp=function(testnull, testdata, numperm=1000){
	testnull=na.omit(testnull)
	test=sapply(testdata, function(x){
		if (is.na(x)){
                  return(NA) 
		} else if (x<min(testnull)){
			return(paste0("<",1/numperm))
		} else {
			return(as.character(table(x<testnull)["FALSE"]/length(na.omit(testnull))))
		}
	})
	return(test)
}

