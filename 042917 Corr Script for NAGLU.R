########################### Loading Libraries #############################################
setwd("/Users/mabelfurutsuki/Desktop/URAPIPY/Check")
library(ROCR)
library(ggplot2)
library(ggvis)
library(corrplot)


########################### Read Data and ROC Parameter Generation ########################
ROCR1 <- read.table("CP.tsv", sep="\t", header=TRUE)
ROCR1 <- head(cbind(ROCR1$P211,  #1
                    ROCR1$P212, 
                    ROCR1$P221, 
                    ROCR1$P231, 
                    ROCR1$P241, #5
                    ROCR1$P251, 
                    ROCR1$P252, 
                    ROCR1$P253, 
                    ROCR1$P261, 
                    ROCR1$P262, #10
                    ROCR1$P263, 
                    ROCR1$P264, 
                    ROCR1$P271, 
                    ROCR1$P281, 
                    ROCR1$P282, #15
                    ROCR1$P291, 
                    ROCR1$P301,
                    ROCR1$PWT, 
                    ROCR1$MutPred2,
                    ROCR1$MutationTaster,
                    ROCR1$Polyphen2_HDIV_score,
                    ROCR1$Polyphen2_HVAR_score,
                    ROCR1$Condel, 
                    ROCR1$MCAP.Score, 
                    ROCR1$LoFtool, 
                    ROCR1$CADD_raw, 
                    ROCR1$MutationTaster_website_raw_score,
                    ROCR1$MutationTaster_converted_rankscore, 
                    ROCR1$SIFT_SCORE, 
                    ROCR1$MutationAssessor_score, 
                    ROCR1$VEST3_score, 
                    ROCR1$SIFT_Binarized, 
                    ROCR1$PROVEAN_SCORE, 
                    ROCR1$GERPRS, 
                    ROCR1$GERPNR, 
                    ROCR1$phyloP46way_placental, 
                    ROCR1$phyloP100way_vertebrate, 
                    ROCR1$SiPhy_29way_logOdds, 
                    ROCR1$LR_score, 
                    ROCR1$RadialSVM_score, 
                    ROCR1$FATHMM_score, 
                    ROCR1$LRT_score, 
                    ROCR1$X1SIFT,
                    ROCR1$relative_activity), 163) #18


colnames(ROCR1) <- c("P211", 
                     "P212", 
                     "P221", 
                     "P231", 
                     "P241", 
                     "P251", 
                     "P252", 
                     "P253", 
                     "P261", 
                     "P262", 
                     "P263", 
                     "P264", 
                     "P271", 
                     "P281", 
                     "P282", 
                     "P291", 
                     "P301",
                     "MutPred2",
                     "MutationTaster_website_raw_score",
                     "MutationTaster_converted_rankscore",
                     "Polyphen2_HDIV_score",
                     "Polyphen2_HVAR_score",
                     "Condel", 
                     "MCAP.Score", 
                     "LoFtool", 
                     "CADD_raw", 
                     "MutationTaster_score", 
                     "SIFT_SCORE", 
                     "MutationAssessor_score", 
                     "VEST3_score", 
                     "SIFT_Binarized", 
                     "PROVEAN_SCORE", 
                     "GERPRS", 
                     "GERPNR", 
                     "phyloP46way_placental", 
                     "phyloP100way_vertebrate", 
                     "SiPhy_29way_logOdds", 
                     "LR_score", 
                     "RadialSVM_score", 
                     "FATHMM_score", 
                     "LRT_score", 
                     "X1SIFT",
                     "relative_activity")
pear <- cor(ROCR1, method='pearson', use="complete")
spear <- cor(ROCR1, method='spearman', use="complete")


png(height=1500, width=1500, pointsize=15, file="NAGLUPearson.png")
corrplot(pear, method="number")
title("Pearson Correlation for NAGLU")

png(height=1500, width=1500, pointsize=15, file="NAGLUSpearman.png")
corrplot(spear, method="number")
title("Spearman Correlation for NAGLU")

