#' Updated grs.plot function from package gtx 
#'
#' Modified function form of the function grs.plot from package gtx
#' that allows coloring by allele frequency. 
#' @param col.by name of column in dataset to color by (for eg: af) 
#' @param display.legend logical input 
#'
#' @export
#'
#' @seealso gtx, grs.plot
#' @examples
#' with(test, grs.plot.new(get(paste("Effect",trait, sep=".")), AdjustedSCD.Effect, StdErr,Freq1, col.by="af"))


grs.plot.new=
function (w, b, s, af, col.by=NULL, text = NULL, textpos = NULL, textcex = 0.5, 
    alpha = 0.05, display.legend=FALSE) 
{
    f <- !is.na(w) & !is.na(b) & !is.na(s) & !is.na(af)
    ws <- sign(w)
    plot((w/ws)[f], (b/ws)[f], xlim = c(0, 1.1 * max((w/ws)[f])), 
        ylim = range(c((b/ws)[f] + qnorm(0.025) * s[f], (b/ws)[f] + 
            qnorm(0.975) * s[f])), type = "n", ann = FALSE, las = 1)
    abline(h = 0, lty = "dotted")
    ahat <- sum((w * b/s^2)[f])/sum((w^2/s^2)[f])
    aSE <- sqrt(1/sum((w^2 * s^-2)[f]))
    abline(a = 0, b = ahat, col = "red")
    abline(a = 0, b = ahat + qnorm(alpha/2) * aSE, col = "red", 
        lty = "dashed")
    abline(a = 0, b = ahat + qnorm(1 - alpha/2) * aSE, col = "red", 
        lty = "dashed")
    for (idx in which(f)) {
        lines(rep((w/ws)[idx], 2), (b/ws)[idx] + qnorm(c(alpha/2, 
            1 - alpha/2)) * s[idx], col = "grey")
    }
    if (!is.null(text)) {
        if (is.null(textpos)) 
            textpos <- rep(1, length(text))
        text((w/ws)[f], (b/ws)[f], text[f], pos = textpos[f], 
            cex = textcex)
    }
	if (!is.null(col.by)) {
		if(col.by=="af"){
			af[af>0.5]=1-af[af>0.5]
			test.col=cut(af, c(0,0.05, 0.1,0.2,0.3, 0.5))
			levels(test.col)=rainbow(5)
			test.col=as.character(test.col)
		} else {
			test.col=as.factor(test[,col.by])
			legend.test=cbind(levels(test.col), rainbow(length(levels(test.col))))
			levels(test.col)=rainbow(length(levels(test.col)))
			test.col=as.character(test.col)
		}
    }
	
	points((w/ws)[f], (b/ws)[f], col=test.col, pch=19)
	if (display.legend){
		if(col.by=="af"){
			legend("bottomright", c("0-0.05","0.05-0.10","0.10-0.20","0.20-0.30","0.30-0.50"), pch=19, bty="n", col=rainbow(5), title="MAF cutoff")
		} else {
			legend("bottomright", legend=legend.test[,1], pch=19, bty="n", col=as.character(legend.test[,2]))
		}
		
	}
	mtext(paste("ahat=", prettyNum(ahat, digits=3)),side=3, line=-1.5,cex=0.8, font=2)
	mtext(paste("PvalDirection=", prettyNum(chisq.test(table(test$Concordance))$p.value, digits=3)),side=1, line=-1.5,cex=0.8, font=2)
}


