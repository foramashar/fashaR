#' Function to read in large dataframes faster
#' 
#' @param filename Name of file to be read in
#' @export
#' @seealso gtx, grs.plot

fastread=function(filename){
	require(sqldf)
	f=file(filename)
	OBJ<- sqldf("select * from f", dbname = tempfile(), file.format = list(header = T, row.names = F, sep="\t"))
	return(OBJ)
}
