#' Function to make the SNP clumps at different Pvalue cutoffs
#' 
#' @param trait Trait to compare SCD results to
#' @param clumpfile File with rsIDs and pvalues for
#' trait of interest. Header MUST be SNP and pvalue
#' @param outfilename Filename for all output files,
#' will be followed by alpha cutoff value
#' @param MAF Maf cutoff to be used for filtering
#' @export
#' @seealso gtx, grs.plot

makeclumps.custom.MAF=function(trait, clumpfile, outfilename, MAF=0.05, RSQ=0.01, DIST=1000){
i=5e-8
	system(paste("plink2 --bfile /dcs01/arking/arkinglab/active/projects/scd.meta/analyses/scd.meta.ver2/ARIC.b35.b37.liftover/aric.f3v2.imputed.b37 --maf ",MAF," --clump ",clumpfile," --clump-p1 ",format(i, scientific=F)," --clump-p2 0.05 --clump-r2 ",ifelse(i==5e-8, 0, 0.05)," --clump-kb ",DIST," --clump-field pvalue --clump-verbose --out test"))
	test=system("more test.clumped | grep RANGE", intern=T)
	test=gsub(" +RANGE: +","",test)
	test=do.call(rbind,strsplit(test, split=":|\\.\\."))
	test=as.data.frame(test)
	test$name=paste("R", 1:nrow(test), sep="")
	write.table(test, "test.intervals.toexclude", col.names=F, row.names=F, sep="\t", quote=F)
	clumpedname=paste(outfilename,format(i, scientific=F),".clumped", sep="")

	system(paste("plink2 --bfile /dcs01/arking/arkinglab/active/projects/scd.meta/analyses/scd.meta.ver2/ARIC.b35.b37.liftover/aric.f3v2.imputed.b37 --maf ",MAF," --clump ", clumpfile," --clump-p1 ",format(i, scientific=F)," --clump-p2 0.05 --clump-r2 ",ifelse(i==5e-8, 0,RSQ)," --clump-kb ",DIST," --clump-field pvalue --out ",outfilename,format(i, scientific=F), sep=""))

	for (i in c(1e-5,1e-3, 0.05, 0.99)){
		if (as.numeric(system(paste("wc -l test.intervals.toexclude | cut -f1 -d' '"), intern=T))==0){
		 system(paste("plink2 --bfile /dcs01/arking/arkinglab/active/projects/scd.meta/analyses/scd.meta.ver2/ARIC.b35.b37.liftover/aric.f3v2.imputed.b37 --maf ",MAF," --clump ", clumpfile,", --clump-p1 ",format(i, scientific=F)," --clump-p2 0.99 --clump-r2 ",RSQ, " --clump-kb ",DIST," --clump-field pvalue --out ",outfilename,format(i, scientific=F), sep=""))
		} else {
			system(paste("plink2 --bfile /dcs01/arking/arkinglab/active/projects/scd.meta/analyses/scd.meta.ver2/ARIC.b35.b37.liftover/aric.f3v2.imputed.b37 --maf ",MAF, " --exclude range test.intervals.toexclude --clump ", clumpfile,", --clump-p1 ",format(i, scientific=F)," --clump-p2 0.99 --clump-r2 ",RSQ," --clump-kb ",DIST," --clump-field pvalue --out ",outfilename,format(i, scientific=F), sep=""))
		}
	}
}
