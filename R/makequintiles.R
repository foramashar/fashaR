#' Function to make quintiles from a column within a dataframe
#'
#' Modified function form of the function grs.plot from package gtx
#' that allows coloring by allele frequency. 
#' @param test Dataframe that contains vector to divide into quintiles
#' @param x Name of column that will be divided into quintiles
#'
#' @export
#'
#' @return output Dataframe with additional columns named quintiles and num.quintiles
#'
#' @examples
#' test2=makequintiles(test, resid.mtDNA)

makequintiles=function(test, x){
	test$quintiles= cut(test[,x], breaks=quantile(test[,x], probs=seq(0,1,by=0.2), na.rm=T))
	test$num.quintiles=test$quintiles
	levels(test$num.quintiles)=c("E","D","C","B","A")
	test$num.quintiles=factor(as.character(test$num.quintiles))
	return(test)
}

