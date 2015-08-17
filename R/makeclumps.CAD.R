#' Function to make the SNP clumps at different Pvalue cutoffs
#' 
#' @param trait Trait to compare CAD results to
#' 
#' @export
#' @seealso gtx, grs.plot

makeclumps.CAD=function(trait){
i=5e-8
	system(paste("plink2 --bfile $ARKINGLAB/active/projects/scd.meta/analyses/scd.meta.ver2/ARIC.b35.b37.liftover/aric.f3v2.imputed.b37 --clump ", paste("CAD.",trait,".overlap.SNP.MIpval",sep="")," --maf 0.01 --clump-p1 ",format(i, scientific=F)," --clump-p2 0.05 --clump-r2 ",ifelse(i==5e-8, 0, 0.05)," --clump-kb 1000 --clump-field pvalue --clump-verbose --out test"))
	test=system("more test.clumped | grep RANGE", intern=T)
	test=gsub(" +RANGE: +","",test)
	test=do.call(rbind,strsplit(test, split=":|\\.\\."))
	test=as.data.frame(test)
	test$name=paste("R", 1:nrow(test), sep="")
	write.table(test, "test.intervals.toexclude", col.names=F, row.names=F, sep="\t", quote=F)

	clumpedname=paste(trait,".cad.",format(i, scientific=F),".clumped", sep="")

	system(paste("plink2 --bfile $ARKINGLAB/active/projects/scd.meta/analyses/scd.meta.ver2/ARIC.b35.b37.liftover/aric.f3v2.imputed.b37 --clump ", paste("CAD.",trait,".overlap.SNP.MIpval",sep="")," --maf 0.01 --clump-p1 ",format(i, scientific=F)," --clump-p2 0.05 --clump-r2 0 --clump-kb 1000 --clump-field pvalue --out ",trait,".cad.",format(i, scientific=F), sep=""))

	for (i in c(1e-5,1e-3, 0.05, 0.99)){
		system(paste("plink2 --bfile $ARKINGLAB/active/projects/scd.meta/analyses/scd.meta.ver2/ARIC.b35.b37.liftover/aric.f3v2.imputed.b37 --maf 0.01 --exclude range test.intervals.toexclude --clump ", paste("CAD.",trait,".overlap.SNP.MIpval",sep=""),", --clump-p1 ",format(i, scientific=F)," --clump-p2 0.99 --clump-r2 0.01 --clump-kb 1000 --clump-field pvalue --out ",trait,".cad.",format(i, scientific=F), sep=""))
	}
}
