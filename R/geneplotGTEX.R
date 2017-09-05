geneplotGTEX=function(gene, log10=FALSE){
	if (!exists("gtex", envir=.GlobalEnv)){
		load("~/CompGenom_PublicData/GTEx/v6p/GTEx_Analysis_v6p_RNA-seq_RNA-SeQCv1.1.8_gene_rpkm.rda")
		gtex=as.data.frame(t(data[,-(1:2)]))
			names(gtex)=data[,1]
			as.character(row.names(gtex)) %>% gsub("\\.","-",.) -> gtex$SAMPID
		gtex.samples=read.table("~/CompGenom_PublicData/GTEx/v6/GTEx_Data_V6_Annotations_SampleAttributesDS.txt", header=T, sep="\t", quote="")
		gtex=merge(gtex, gtex.samples, by="SAMPID")
	}
	if (!exists("gtf", envir=.GlobalEnv)){
		gtf=read.table("~/CompGenom_PublicData/GENCODE/gencode.v19.annotation.gtf", sep="\t")
		names(gtf)=c("Chr","source","feature","start","end","score","strand","frame","attribute")
		subset(gtf, feature=="gene") -> gtf
		gtf$attribute %>% strsplit(.,";") -> test
			lapply(test, "[[",1)  %>% gsub("gene_id ","",.) -> gtf$Ensembl.gene_id
			lapply(test, "[[",5) %>% gsub(" gene_name ","",.)-> gtf$gene_name
	}
	genename=gtf[gtf$Ensembl.gene_id==gene,"gene_name"]
	genetitle=paste0("Expression~of~", genename)
	###Plotting all expression
	if (log10){
		gtex %>%
		group_by(SMTSD) %>%
		dplyr::select(one_of(gene), SMTS) %>%
		as.data.frame %>%
		ggplot(aes(x=SMTSD, y=log10(get(gene)), fill=SMTS))+
			geom_boxplot()+
			theme_bw()+
			ylab(paste("Expression of",genename,"\n[in log10(RPKM)]"))+
			xlab("")+
			ggtitle(parse(text=genetitle))+
			theme(axis.text.x  = element_text(angle=45, vjust=1, hjust = 1, size=8))+
			theme(legend.position="none") -> testplot
		print(testplot)	
	}
	else {
		gtex %>%
			group_by(SMTSD) %>%
			dplyr::select(one_of(gene), SMTS) %>%
			as.data.frame %>%
			ggplot(aes(x=SMTSD, y=get(gene), fill=SMTS))+
				geom_hline(yintercept = 10, linetype="dotted") +
				geom_boxplot()+
				theme_bw()+
				ylab(paste("Expression of",genename,"\n[in RPKM]"))+
				xlab("")+
				ggtitle(parse(text=genetitle))+
				theme(axis.text.x  = element_text(angle=45,vjust=1, hjust = 1, size=10 ))+
				theme(legend.position="none") -> testplot
		print(testplot)
	}
}