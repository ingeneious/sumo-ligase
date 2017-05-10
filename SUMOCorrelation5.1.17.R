########################### Loading Libraries #############################################
setwd("/Users/mabelfurutsuki/Desktop/URAPIPY/Check")

library(ROCR)
library(ggplot2)
library(ggvis)
library(corrplot)


########################### Read Data and ROC Parameter Generation ########################
ROCR1 <- read.table("SUMO.tsv", sep="\t", header=TRUE)
#data.matrix(ROCR1, rownames.force = NA)
ROCR1 <- head(cbind(ROCR1$Growth.Score,
	                ROCR1$Growth_Binarized_0.3,
	                ROCR1$Growth_Binarized_0.6,           
                    ROCR1$MutPred2,
                    ROCR1$Condel,                            
                    ROCR1$Provean_Score,         
                    ROCR1$SIFT_score_Kggseq,              
                    ROCR1$X1_SIFT_WANNOVAR,
                    ROCR1$SIFT_Binarized,                    
                    ROCR1$MCAP_score,
                    ROCR1$SIFT_score_Wannovar,              
                    ROCR1$Polyphen2_HDIV_score,
                    ROCR1$Polyphen2_HVAR_score,             
                    ROCR1$LRT_score,    
                    ROCR1$MutationTaster_score,
                    ROCR1$MutationTaster_converted_rankscore,
                    ROCR1$MutationAssessor_score,         
                    ROCR1$FATHMM_score,                     
                    ROCR1$RadialSVM_score,                 
                    ROCR1$LR_score,                       
                    ROCR1$VEST3_score,    
                    ROCR1$CADD_raw...20...D,
                    ROCR1$CADD_phred,                   
                    ROCR1$GERP..x_NR,
                    ROCR1$GERP..x_RS,                 
                    ROCR1$phyloP46way_placental,
                    ROCR1$phyloP100way_vertebrate,        
                    ROCR1$SiPhy_29way_logOdds), 226)

ROCR1
sapply(ROCR1, is.numeric)
ncol(ROCR1)
colnames(ROCR1) <- c("Growth.Score",   
       				 "Growth_Binarized_0.3",
					 "Growth_Binarized_0.6",           
                     "MutPred2",
                     "Condel",                            
                     "Provean_Score",         
                     "SIFT_score_Kggseq",              
                     "X1_SIFT_WANNOVAR",
                     "SIFT_Binarized",                    
                     "MCAP_score",
                     "SIFT_score_Wannovar",              
                     "Polyphen2_HDIV_score",
                     "Polyphen2_HVAR_score",             
                     "LRT_score",    
                     "MutationTaster_score",
                     "MutationTaster_converted_rankscore",
                     "MutationAssessor_score",         
                     "FATHMM_score",                     
                     "RadialSVM_score",                 
                     "LR_score",                       
                     "VEST3_score",    
                     "CADD_raw...20...D",
                     "CADD_phred",                   
                     "GERP..x_NR",
                     "GERP..x_RS",                 
                     "phyloP46way_placental",
                     "phyloP100way_vertebrate",        
                     "SiPhy_29way_logOdds")


pear <- cor(ROCR1, method='pearson', use="complete")
spear <- cor(ROCR1, method='spearman', use="complete")

png(height=1500, width=1500, pointsize=15, file="SUMOPearson.png")
corrplot(pear, method="number")
title("Pearson Correlation for SUMO")    

png(height=1500, width=1500, pointsize=15, file="SUMOSpearman.png")
corrplot(spear, method="number")
title("Spearman Correlation for SUMO") 
