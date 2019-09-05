library("biomaRt")
library("csbl.go")
library("protr")
ensembl = useEnsembl(biomart="ensembl", dataset="hsapiens_gene_ensembl");

GenAge_CE = read.table(file="F:/publications/MedChemComm/data/GenAge_CE.txt",sep="\t",header=TRUE,quote="")

entrez_WBtranscript = getBM(attributes=c("entrezgene","ensembl_peptide_id"), 
                                  filters="ensembl_peptide_id",values=data1$V1, mart=ensembl)

trascript_GOterm = getBM(attributes=c("peptide","ensembl_peptide_id"), 
                            filters="ensembl_peptide_id",values=entrez_WBtranscript$wormbase_transcript, mart=ensembl)

transcript_peptide = getBM(attributes=c("peptide","wormbase_transcript"), 
                                  filters="wormbase_transcript",values=entrez_WBtranscript$wormbase_transcript, mart=ensembl)

unique_transcript = unique(trascript_GOterm$wormbase_transcript);
#GOlist=list();
for(i in 1:length(unique_transcript)){
  transcript = unique_transcript[i];
  GOterm_indx = grep(transcript,trascript_GOterm$wormbase_transcript);
  cat(transcript,trascript_GOterm$go_id[GOterm_indx],'\n',file="F:/publications/MedChemComm/data/GOlist.txt",append=TRUE);
}

# compute GO similarity using csbl.go package
set.prob.table(organism=TAXONOMY.C_ELEGANS, type="similarity")
ent = entities.from.text("F:/publications/MedChemComm/data/GOlist.txt")
GO_similarity = entity.sim.many(ent, "MF", "Resnik")

# compute GO similarity using protr package
genelist = entrez_id$entrezgene
#gsimmat2 = parGOSim(genelist, type = 'gene', ont = "MF", organism = "worm")
d = godata('org.Hs.eg.db', ont="MF", computeIC=FALSE)
gsimmat2 = mgeneSim(genelist, semData=d, measure="Wang")

# compute protein sequence similarity using protr package
plist=as.list(transcript_peptide$peptide);
psimmat = parSeqSim(plist, cores = 4, type = 'local', submat = 'BLOSUM62')


# compute cosine similarity based on chemical structure
chemical_attribute = chemical_feature[,2:1445];
chemical_fingerprint = chemical_feature[,1446:2326];

rownames(chemical_attribute) = chemical_feature[,1];
rownames(chemical_fingerprint) = chemical_feature[,1];

compound_num = dim(chemical_fingerprint)[1];

for(i in 1:dim(chemical_attribute)[2]){
  vec = chemical_attribute[,i];
  vec[which(is.infinite(vec))] = max(vec,na.rm=TRUE);
  vec[which(is.na(vec))] = mean(vec,na.rm=TRUE);
  vec = vec-mean(vec);
  chemical_attribute[,i] = vec/sqrt(sum(vec*vec));
}

chemical_fingerprint[is.na(chemical_fingerprint)] <- 0

library(foreach)
library(doParallel)
cl <- makeCluster(3)
registerDoParallel(cl)
chemattr_sim <- foreach(i=1:compound_num,.combine='rbind') %dopar% Jaccard_similarity(i)
stopCluster(cl)

library(philentropy)
chemattr_sim <- distance(chemical_fingerprint,"jaccard")



human_idmapping = getBM(attributes=c("ensembl_gene_id","ensembl_transcript_id","ensembl_peptide_id"), 
                            mart=ensembl)






