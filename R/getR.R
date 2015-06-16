#' Function to get sum of probes A+B from Affy 6.0 array
#'
#' Function to return A+B probe intensities from matrix with 
#' A and B intensities
#' @param x Matrix with rows as samples and columns as probe intensities
#'
#' @export
#'
#' @return output Matrix with A+B probe intensities; same number of
#' rows as input, half as many columns
#'
#' @examples
#' test2=getR(test)

getR=function(x){
	y=matrix(NA, nrow=nrow(x), ncol=ncol(x)/2)
	oddnos=seq(1,ncol(x), by=2)
	for(i in oddnos){
		y[,oddnos==i]=x[,i]+x[,i+1]
	}
	row.names(y)=row.names(x)
	return(y)
}

