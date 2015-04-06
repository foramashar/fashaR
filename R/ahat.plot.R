#' Function to make the Ahat plot for genetic enrichment analyses
#' Builds on grs.summary function from package gtx
#' 
#' @param x Input data frame of results from grs.summary at different Pval cutoffs.
#' @param genderdiffvector Optional list of pvalue for difference in ahat by sex.
#' 
#' @export
#' @seealso gtx, grs.plot

ahat.plot=function (x,genderdiffvector, text.cex=0.9){
	test=as.data.frame(t(x))
	test$group=gsub("\\..+","",row.names(test))
	ylimmax=max(test$ahat+qnorm(1-0.05/2)*test$aSE)
	ylimmin=min(test$ahat-qnorm(1-0.05/2)*test$aSE)*2
	testcombined=with(subset(test,group=="combined"), rbind(m, prettyNum(ahat, digits=2), prettyNum(R2rs, digits=2),  prettyNum(pval, digits=3),prettyNum(phet, digits=3)))
	if ("male" %in% test$group) {
		testmale=with(subset(test,group=="male"), rbind(m, prettyNum(ahat, digits=2), prettyNum(R2rs, digits=2),  prettyNum(pval, digits=3),prettyNum(phet, digits=3)))
		testfemale=with(subset(test,group=="female"), rbind(m, prettyNum(ahat, digits=2), prettyNum(R2rs, digits=2),  prettyNum(pval, digits=3),prettyNum(phet, digits=3)))
	}
	testgenderdiff=as.matrix(round(genderdiffvector,3))
	test.table=rbind(testcombined, testmale, testfemale,testgenderdiff)
	row.names(test.table)=c(rep(c("m","ahat","R2rs","pval","phet"),3), "GenderP")
	colnames(test.table)=gsub(paste("combined",trait,"scd.", sep="."),"",row.names(subset(test[order(test$m),], group=="combined")))
	with(subset(test[order(test$m),], group=="combined"), {
		axislabels=gsub(paste("combined",trait,"scd.", sep="."),"",row.names(subset(test[order(test$m),], group=="combined")))
		plot(ahat, pch=19, col="forestgreen",ylim=c(ylimmin, ylimmax), xlim=c(0.5,length(axislabels)+1),xlab="Alpha Cutoff", cex.lab=1.2,xaxt="n",main=paste("GRS--",toupper(trait),"in SCD"))
		axis(1,at=(1:length(axislabels))+0.2,labels=axislabels)
		for (i in 1:length(axislabels)) {
			lines(cbind(rep(i, 2), ahat[i] + qnorm(c(0.05/2, 1-0.05/2)) * aSE[i]), col = "grey")
		}
	})
	abline(h=0, lty="dotted",col="grey")
	with(subset(test[order(test$m),],group=="male"), {
		axislabels=gsub(paste("male",trait,"scd.", sep="."),"",row.names(subset(test[order(test$m),], group=="male")))
		points((1:length(axislabels))+0.2,ahat, pch=19, col="blue")
		for (i in 1:length(axislabels)) {
			lines(cbind(rep(i+0.2, 2), ahat[i] + qnorm(c(0.05/2, 1-0.05/2)) * aSE[i]), col = "grey")
		}
	})
	with(subset(test[order(test$m),],group=="female"), {
		axislabels=gsub(paste("female",trait,"scd.", sep="."),"",row.names(subset(test[order(test$m),], group=="female")))
		points((1:length(axislabels))+0.4,ahat, pch=19, col="deeppink")
		for (i in 1:length(axislabels)) {
			lines(cbind(rep(i+0.4, 2), ahat[i] + qnorm(c(0.05/2, 1-0.05/2)) * aSE[i]), col = "grey")
		}
	})
        addtable2plot.new("bottom", table=test.table, display.rownames=T,display.colnames=T, hlines=T, bg=c(rep(c("forestgreen","blue","deeppink"),each=(nrow(test.table)-1)/3),"white"), text.col= c(rep("white", (nrow(test.table)-1)),"black"), cex=text.cex, xpad=0.5)
	return(test.table)
}
