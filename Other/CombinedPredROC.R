#-----------------------SETUP-----------------
setwd("/Users/mabelfurutsuki/Desktop/URAPIPY/Check")
library(ROCR)
library(ggplot2)
library(plotROC)
library(gridExtra)
require(grid)
plotlist <<- list()
ROCR1 <- read.table("CP.tsv", sep="\t", header=TRUE)

#----Note, this is not assigned. This is the Compiled Predictions set for Naglu
# head(cbind(ROCR1$P211,  #1
#            ROCR1$P212, 
#            ROCR1$P221, 
#            ROCR1$P231, 
#            ROCR1$P241, #5
#            ROCR1$P251, 
#            ROCR1$P252, 
#            ROCR1$P253, 
#            ROCR1$P261, 
#            ROCR1$P262, #10
#            ROCR1$P263, 
#            ROCR1$P264, 
#            ROCR1$P271, 
#            ROCR1$P281, 
#            ROCR1$P282, #15
#            ROCR1$P291, 
#            ROCR1$P301,
#            ROCR1$PWT, 
#            ROCR1$MutPred2,
#            ROCR1$MutationTaster,
#            ROCR1$Polyphen2_HDIV_score,
#            ROCR1$Polyphen2_HVAR_score,
#            ROCR1$Condel, 
#            ROCR1$MCAP.Score, 
#            ROCR1$LoFtool, 
#            ROCR1$CADD_raw, 
#            ROCR1$MutationTaster_website_raw_score,
#            ROCR1$MutationTaster_converted_rankscore, 
#            ROCR1$SIFT_SCORE, 
#            ROCR1$MutationAssessor_score, 
#            ROCR1$VEST3_score, 
#            ROCR1$relative_activity, 
#            ROCR1$SIFT_Binarized, 
#            ROCR1$PROVEAN_SCORE, 
#            ROCR1$GERPRS, 
#            ROCR1$GERPNR, 
#            ROCR1$phyloP46way_placental, 
#            ROCR1$phyloP100way_vertebrate, 
#            ROCR1$SiPhy_29way_logOdds, 
#            ROCR1$LR_score, 
#            ROCR1$RadialSVM_score, 
#            ROCR1$FATHMM_score, 
#            ROCR1$LRT_score, 
#            ROCR1$X1SIFT), 163) #18

i <<- 1

#_________________ROC CURVES y^____________________________________
myfunction <- function(testingMethodString){
  ROCR1$is_PWT <- ROCR1$PWT > 0.1
  is_test <- runif(nrow(ROCR1)) > 0.5
  train <- ROCR1[is_test==FALSE,]
  test <- ROCR1[is_test==TRUE,]

  fla <- paste("is_PWT ~", paste(testingMethodString)) #create a formula
  summary(fit <- glm(as.formula(fla), data=train))
  
  prob <- predict(fit, newdata=test, type="response")
  pred <- prediction(prob, test$is_PWT)
  perf <- performance(pred, measure = "tpr", x.measure = "fpr")
  
  auc <- performance(pred, measure = "auc")
  auc <- auc@y.values[[1]]
  roc.data <- data.frame(fpr=unlist(perf@x.values),
                         tpr=unlist(perf@y.values),
                         model="GLM")
  filename <- paste(testingMethodString, "ROC.png")
  png(height=500, width=500, file=filename)
  
  pname <- paste0("Plot-", testingMethodString) #MW
  p <<- ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
    geom_ribbon(alpha=0.2) +
    geom_line(aes(y=tpr)) +
    ggtitle(paste0(testingMethodString, "\n ROC Curve w/ AUC=", round(auc, digits = 3))) +
    theme(plot.title = element_text(size = 8, face = "bold"))
  ggsave(paste0(pname,".png"),p)
  plotlist[[i]] <<- p 
  i <<- i + 1
  return(auc)
}

mylist <- c("P211", 
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
"relative_activity", 
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
"X1SIFT")

for(group in mylist) {
  myfunction(group)
}
pdf("filename.pdf", width = 50, height = 50) #new
p <- grid.arrange(grobs=plotlist,ncol=8,nrow=8, top=textGrob("Naglu Plots", gp=gpar(fontsize=20,font=3)))
dev.off()

#ggsave("bigplot.png",p)
#Code from http://blog.yhat.com/posts/roc-curves.html 
#As.formula advice from http://stackoverflow.com/questions/3588961/specifying-formula-in-r-with-glm-without-explicit-declaration-of-each-covariate
  
