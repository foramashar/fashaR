#' Function to get logical vector for if alleles match
#'
#' Function to return logical vector as response to if alleles in
#' two vector of alleles match. Takes into account A/T and C/G matching
#' but does NOT account for snps with only A/T or C/G alleles.
#'
#' @param x,y Vectors of alleles 
#'
#' @export
#'
#' @return output Logical vector of length x 
#'
#' @examples
#' Match=allelematch(scd$Allele1.scd, scd$Allele1.cad)

allelematch=function(x,y){
	if (length(x)!=length(y)){cat("X and Y vectors do NOT have same length!"); next}
	x=toupper(x)
	y=toupper(y)
	z=(x==y) | (x=="A" & y=="T") | (x=="C" | y=="G") | (x=="G" | y=="C") | (x=="T" | y=="A")
	return(z)
}
