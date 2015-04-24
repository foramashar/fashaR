#' Function to tidy up results from CoxPH model 
#'
#' @param coxph.model An object of class coxph from package survival
#' @param rowname.suffix Suffix to add to rownames to resulting table 
#' @param colname.suffix Optional argument to add suffix to column names of resulting table 
#' @param confint Logical argument to determine whether confindence intervals need to be included in the output
#' @export
#'
#' @return output Dataframe with summary of results form a coxph object
#' @examples
#' test.coxph=coxph(Surv(DNA.visit.FUTIME.DTH, DEAD12) ~ GENDER+resid.mtDNA+DNA.visit.AGE+CENTER, data=test)
#' test.results=CoxResult(test.coxph, "all")

CoxResult=function(coxph.model, rowname.suffix, colname.suffix=NULL, confint=FALSE){
	require(survival)
	results=coef(summary(coxph.model))
	if (confint==TRUE){results=cbind(results, summary(coxph.model)$conf.int[,3:4],coxph.model$n,coxph.model$nevent)}
	else {
		test.n=paste(coxph.model$n, " (",coxph.model$nevent,")", sep="" )
		results=cbind(results, test.n)
	}
	row.names(results)=paste(row.names(results),rowname.suffix, sep=".")
	if (!is.null(colname.suffix)){
		colnames(results)=paste(colnames(results),colname.suffix, sep=".")
	}
	return(results)
}

