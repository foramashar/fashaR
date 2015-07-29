#' Function to convert vector to class numeric 
#'
#' @param x An input vector to be converted to class numeric
#'
#' @return output Numeric vector with non-numeric values replaced with NA
#'
#' @keywords factor numeric
#'
#' @export
#'
#' @examples
#' test=factor(c("1","4","ABC","5"))
#' print(as.numeric(test))
#' print(factor2num(test))

factor2num=function(x){
	x=as.numeric(as.character(x))
	return(x)
}