#' Plots grid of PCs
#' @param data name of file to be read in 
#' @return a data frame 
#' @import janitor
#' @export
#' @examples

read.identified=function(data){
	read.table(data, sep="\t") -> test
	names(test)=c("BED Chromosome","BED Min.Position","BED Max.Position","BED Name","Filename","WindowIndex","Chromosome","Position	Sequence","plus.mi","minus.mi","bi.sum.mi","bi.geometric_mean.mi","plus.total","minus.total","total.sum","total.geometric_mean","primer1.mi","primer2.mi","primer.geometric_mean","position.stdev","Off-Target Sequence","Mismatches","Length","BED off-target","Chromosome	BED","off-target start","BED off-target end","BED off-target name","BED Score","Strand","Cells","Targetsite","Target Sequence")
	require(janitor)
	clean_names(test) ->test
	return(test)
	}