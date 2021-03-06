#' Function to tidy up results from CoxPH model 
#'
#' @param coxph.model An object of class coxph from package survival
#' @param rowname.suffix Suffix to add to rownames to resulting table 
#' @param colname.suffix Optional argument to add suffix to column names of resulting table 
#' @param confint Logical argument to determine whether confindence intervals need to be included in the output
#' @param prettyDig Optional numeric input to indicate number of significant digits in the output table
#' @export
#'
#' @return output Dataframe with summary of results form a coxph object
#' @examples
#' test.coxph=coxph(Surv(DNA.visit.FUTIME.DTH, DEAD12) ~ GENDER+resid.mtDNA+DNA.visit.AGE+CENTER, data=test)
#' test.results=CoxResult(test.coxph, "all", prettyDig=3)

CoxResult=function(coxph.model, rowname.suffix, colname.suffix=NULL, confint=FALSE, prettyDig=NULL){
        require(survival)
        results=coef(summary(coxph.model))
        if (confint==TRUE){results=cbind(results, summary(coxph.model)$conf.int[,3:4])}
		
        if (!is.null(prettyDig)){
                test.results=apply(results, 2, function(x){prettyNum(as.numeric(x),digits=prettyDig)})
        }
		row.names(test.results)=row.names(results)
		results=test.results
        test.n=paste(coxph.model$n, " (",coxph.model$nevent,")", sep="" )
        results=cbind(results, test.n)
        row.names(results)=paste(row.names(results),rowname.suffix, sep=".")
        if (!is.null(colname.suffix)){
                colnames(results)=paste(colnames(results),colname.suffix, sep=".")
        }
        return(results)
}
