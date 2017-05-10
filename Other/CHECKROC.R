#-----------------------SETUP-----------------
setwd("/Users/mabelfurutsuki/Desktop/URAPIPY/Check")
library(ROCR)
ROCR1 <- read.table("CP.tsv", sep="\t", header=TRUE)

#----Names have been updated for SUMO rather than NAGLU. This binds the table columns I want
head(cbind(ROCR1$P211,  #1
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
           ROCR1$PWT), 163) #18

#Set up prediction functions for all of the softwares
pred211 <- prediction(ROCR1$P211, ROCR1$PWT)
pred212 <- prediction(ROCR1$P212, ROCR1$PWT)
pred221 <- prediction(ROCR1$P211, ROCR1$PWT)
pred231 <- prediction(ROCR1$P231, ROCR1$PWT)
pred241 <- prediction(ROCR1$P241, ROCR1$PWT)
pred251 <- prediction(ROCR1$P251, ROCR1$PWT)
pred252 <- prediction(ROCR1$P252, ROCR1$PWT)
pred253 <- prediction(ROCR1$P253, ROCR1$PWT)
pred261 <- prediction(ROCR1$P261, ROCR1$PWT)
pred262 <- prediction(ROCR1$P262, ROCR1$PWT)
pred263 <- prediction(ROCR1$P263, ROCR1$PWT)
pred264 <- prediction(ROCR1$P264, ROCR1$PWT)
pred271 <- prediction(ROCR1$P271, ROCR1$PWT)
pred281 <- prediction(ROCR1$P281, ROCR1$PWT)
pred282 <- prediction(ROCR1$P282, ROCR1$PWT)
pred291 <- prediction(ROCR1$P291, ROCR1$PWT)
pred301 <- prediction(ROCR1$P301, ROCR1$PWT)


#Put all the prediction function outputs into array
predarray <- c(pred211,
               pred212,
               pred221,
               pred231,
               pred241,
               pred251,
               pred252,
               pred253,
               pred261,
               pred262,
               pred263,
               pred264,
               pred271,
               pred281,
               pred282,
               pred291,
               pred301)


#This is to store all the auc.perfs (to be looped over later)
auc.perf1 <- vector()


#--------------------Repeated Actions. ROCR done here----------------
for (pred in predarray) { 
  class(pred)
  slotNames(pred)
  sn = slotNames(pred)
  sapply(sn, function(x) length(slot(pred, x)))
  sapply(sn, function(x) class(slot(pred, x)))
  
  auc.perf = performance(pred, measure = "auc")
  auc.perf1 <- append(auc.perf1, auc.perf@y.values)
}


#------------ TONS OF PLOTTING ---------------
roc.perf1=performance(pred211,measure="tpr",x.measure="fpr")
roc.perf2=performance(pred212,measure="tpr",x.measure="fpr")
roc.perf3=performance(pred221,measure="tpr",x.measure="fpr")
roc.perf4=performance(pred231,measure="tpr",x.measure="fpr")
roc.perf5=performance(pred241,measure="tpr",x.measure="fpr")
roc.perf6=performance(pred251,measure="tpr",x.measure="fpr")
roc.perf7=performance(pred252,measure="tpr",x.measure="fpr")
roc.perf8=performance(pred253,measure="tpr",x.measure="fpr")
roc.perf9=performance(pred261,measure="tpr",x.measure="fpr")
roc.perf10=performance(pred262,measure="tpr",x.measure="fpr")
roc.perf11=performance(pred263,measure="tpr",x.measure="fpr")
roc.perf12=performance(pred264,measure="tpr",x.measure="fpr")
roc.perf13=performance(pred271,measure="tpr",x.measure="fpr")
roc.perf14=performance(pred281,measure="tpr",x.measure="fpr")
roc.perf15=performance(pred282,measure="tpr",x.measure="fpr")
roc.perf16=performance(pred291,measure="tpr",x.measure="fpr")
roc.perf17=performance(pred301,measure="tpr",x.measure="fpr")


par(mfrow=c(5, 5), mar = rep(2, 4))
plot(roc.perf1,main=c(paste("pred211",round(as.numeric(auc.perf1[1]),digits=3))))
abline(a=0,b=1)
plot(roc.perf2,main=c(paste("pred212",round(as.numeric(auc.perf1[2]),digits=3))))
abline(a=0,b=1)
plot(roc.perf3,main=c(paste("pred221",round(as.numeric(auc.perf1[3]),digits=3))))
abline(a=0,b=1)
plot(roc.perf4,main=c(paste("pred231",round(as.numeric(auc.perf1[4]),digits=3))))
abline(a=0,b=1)
plot(roc.perf5,main=c(paste("pred241",round(as.numeric(auc.perf1[5]),digits=3))))
abline(a=0,b=1)
plot(roc.perf6,main=c(paste("pred251",round(as.numeric(auc.perf1[6]),digits=3))))
abline(a=0,b=1)
plot(roc.perf7,main=c(paste("pred252",round(as.numeric(auc.perf1[7]),digits=3))))
abline(a=0,b=1)
plot(roc.perf8,main=c(paste("pred253",round(as.numeric(auc.perf1[8]),digits=3))))
abline(a=0,b=1)
plot(roc.perf9,main=c(paste("pred261",round(as.numeric(auc.perf1[9]),digits=3))))
abline(a=0,b=1)
plot(roc.perf10,main=c(paste("pred262",round(as.numeric(auc.perf1[10]),digits=3))))
abline(a=0,b=1)
plot(roc.perf11,main=c(paste("pred263",round(as.numeric(auc.perf1[11]),digits=3))))
abline(a=0,b=1)
plot(roc.perf12,main=c(paste("pred264",round(as.numeric(auc.perf1[12]),digits=3))))
abline(a=0,b=1)
plot(roc.perf13,main=c(paste("pred271",round(as.numeric(auc.perf1[13]),digits=3))))
abline(a=0,b=1)
plot(roc.perf14,main=c(paste("pred281",round(as.numeric(auc.perf1[14]),digits=3))))
abline(a=0,b=1)
plot(roc.perf15,main=c(paste("pred282",round(as.numeric(auc.perf1[15]),digits=3))))
abline(a=0,b=1)
plot(roc.perf16,main=c(paste("pred291",round(as.numeric(auc.perf1[16]),digits=3))))
abline(a=0,b=1)
plot(roc.perf17,main=c(paste("pred301",round(as.numeric(auc.perf1[17]),digits=3))))
abline(a=0,b=1)
