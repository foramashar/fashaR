#' Function to check if object is of class character
#' and of length 0
#'
#' @param x An input vector to be checked
#'
#' @return output Logical output 
#'
#' @keywords character zero
#'
#' @export


is.character0=function(x){is.character(x) && length(x)==0L}
