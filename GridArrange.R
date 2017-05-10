setwd("/Users/mabelfurutsuki/Desktop/URAPIPY/Check")
library(ROCR)
library(ggplot2)
library(plotROC)
library(gridExtra)
require(grid)
plotlist <<- list()

ROCR1 <- read.table("CP.tsv", sep="\t", header=TRUE)
head(cbind(ROCR1$P211,  #1
           ROCR1$P212,
           ROCR1$relative_activity))
i <<- 1
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
            "P212", "relative_activity")
for(group in mylist) {
  myfunction(group)
}
p <- grid.arrange(grobs=plotlist,ncol=3,nrow=3, top=textGrob("Naglu Plots", gp=gpar(fontsize=20,font=3)))
ggsave("bigplot.png",p)