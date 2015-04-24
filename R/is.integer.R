#' Function to check if object is an integer of length 0
#'
#' @param x An input vector to be checked
#'
#' @return output Logical output 
#'
#' @keywords integer zero
#'
#' @export

is.integer0 <- function(x){is.integer(x) && length(x) == 0L}
