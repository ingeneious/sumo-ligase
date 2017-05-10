#-----------------------SETUP-----------------
setwd("/Users/mabelfurutsuki/Desktop/URAPIPY/Check")
library(ROCR)
library(ggplot2)
library(plotROC)
library(gridExtra)
require(grid)
plotlist <<- list()
ROCR1 <- read.table("SUMO.tsv", sep="\t", header=TRUE)
i <<- 1


myfunction <- function(testingMethodString){
  ROCR1$is_PWT <- ROCR1$Growth_Binarized_0.6
  is_test <- runif(nrow(ROCR1)) > 0.5
  train <- ROCR1[is_test==FALSE,]
  test <- ROCR1[is_test==TRUE,]
  
  fla <- paste("is_PWT ~", paste(testingMethodString)) #create a formula
  summary(fit <- glm(as.formula(fla), data=train))
  
  prob <- predict(fit, newdata=test, type="response")
  pred <- prediction(prob, test$is_PWT)
  print(pred)
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

mylist <- c("Growth.Score",              
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


for(group in mylist) {
  myfunction(group)
}
pdf("SUMOPLOTS.pdf", width = 50, height = 50) #new
p <- grid.arrange(grobs=plotlist,ncol=5,nrow=6, top=textGrob("SUMO PLOTS", gp=gpar(fontsize=20,font=3)))
dev.off()

