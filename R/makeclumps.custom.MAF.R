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

makeclumps.custom.MAF=function(trait, clumpfile, outfilename, MAF=0.05){
i=5e-8
	system(paste("plink2 --bfile /dcs01/arking/arkinglab/active/projects/scd.meta/analyses/scd.meta.ver2/ARIC.b35.b37.liftover/aric.f3v2.imputed.b37 --maf ",MAF," --clump ",clumpfile," --clump-p1 ",format(i, scientific=F)," --clump-p2 0.05 --clump-r2 ",ifelse(i==5e-8, 0, 0.05)," --clump-kb 1000 --clump-field pvalue --clump-verbose --out ",outfilename,format(i, scientific=F), sep=""))
		clumpedname=paste(outfilename,format(i, scientific=F),".clumped", sep="")
		intervalname=gsub("clumped","intervals.toexclude", clumpedname)
		system(paste("more ",clumpedname," | grep RANGE | sed 's:\\s\\+RANGE\\: chr::g' | sed 's:\\::\\t:g' | sed 's:\\.\\.:\\t:g' > ", intervalname))

	system(paste("plink2 --bfile /dcs01/arking/arkinglab/active/projects/scd.meta/analyses/scd.meta.ver2/ARIC.b35.b37.liftover/aric.f3v2.imputed.b37 --maf ",MAF," --clump ", clumpfile," --clump-p1 ",format(i, scientific=F)," --clump-p2 0.05 --clump-r2 ",ifelse(i==5e-8, 0, 0.05)," --clump-kb 1000 --clump-field pvalue --out ",outfilename,format(i, scientific=F), sep=""))
		system(paste("more ",clumpedname," | sed 's:\\s\\+:\\t:g' | cut -f4 | sed '1d'> test",sep=""))
		system(paste("paste ",intervalname," test > test2", sep=""))
		system(paste("mv test2 	", intervalname, sep=""))

	for (i in c(1e-5,1e-3, 0.05, 0.99)){
		if (as.numeric(system(paste("wc -l", intervalname)))==0){
		 system(paste("plink2 --bfile /dcs01/arking/arkinglab/active/projects/scd.meta/analyses/scd.meta.ver2/ARIC.b35.b37.liftover/aric.f3v2.imputed.b37 --maf ",MAF," --clump ", clumpfile,", --clump-p1 ",format(i, scientific=F)," --clump-p2 0.99 --clump-r2 0.01 --clump-kb 1000 --clump-field pvalue --out ",outfilename,format(i, scientific=F), sep=""))
		} else {
			system(paste("plink2 --bfile /dcs01/arking/arkinglab/active/projects/scd.meta/analyses/scd.meta.ver2/ARIC.b35.b37.liftover/aric.f3v2.imputed.b37 --maf ",MAF, " --exclude range ", intervalname," --clump ", clumpfile,", --clump-p1 ",format(i, scientific=F)," --clump-p2 0.99 --clump-r2 0.01 --clump-kb 1000 --clump-field pvalue --out ",outfilename,format(i, scientific=F), sep=""))
		}
	}
}
