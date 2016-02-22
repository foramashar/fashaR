#' Function to increase width of R on linux
#' @export
#' @return output Width of the window is changed
#'

wideScreen=function(howWide=as.numeric(strsplit(system('stty size', intern=T), ' ')[[1]])[2]) {
   options(width=as.integer(howWide))
}
