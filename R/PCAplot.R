#' Plots grid of PCs
#' @param data dataframe with PCs and colorvector
#' @param colorvector name of column by which points should be colored
#' @param numPCs how many PCs should be plotted, must be even number (default=8)
#' @param label_by name of column by which points should be labeled
#' @param pointsize The size of the point for each datapoint
#' @param colorpalette Character vector of colors, must be without names
#' @return a ggplot2 plot object
#' @import ggplot2 cowplot
#' @importFrom reshape2 melt
#' @export
#' @examples


PCAplot=function(data=data,colorvector=NULL, numPCs=8, pointsize=6, colorpalette=NULL){
	require(ggplot2)
	require(cowplot)

	if (numPCs%%2 !=0) {stop("numPCs is not an even number!")}
	# if (!is.null(colorpalette)){palette(colorpalette)}
	testplotlist = list()
	for (pc in seq(1, numPCs, by=2)){
		x=paste0("PC",pc)
		y=paste0("PC",pc+1)
		ggplot(data=data)+
		geom_point( aes_string(x=x, y=y,col=colorvector), size=pointsize)+
		theme_minimal()+
		theme(legend.position="none") -> PC.test

		###If palette provided, add that in
		if (!is.null(colorpalette)){
			if (!is.null(names(colorpalette))){stop("colorpalette must be a vector of colors without names!")}

			PC.test + scale_color_manual(values=colorpalette) -> PC.test
		}

		### Add legend to last plot 
		if (!is.null(colorvector) & (pc+1 == numPCs)) {
			PC.test + theme(legend.position=c(0.8,1)) -> PC.test
		}

		testplotlist[[paste0(x, ".plot")]] <- PC.test
	}
	plot_grid(plotlist=testplotlist, ncol=2)
}

