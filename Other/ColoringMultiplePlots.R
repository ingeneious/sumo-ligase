set.seed(2529)
D.ex <- rbinom(20, size = 1, prob = .5)
M1 <- rnorm(20, mean = D.ex, sd = .65)
M2 <- rnorm(20, mean = D.ex, sd = 1.5)

test <- data.frame(D = D.ex, 
                   M1 = M1, M2 = M2, stringsAsFactors = FALSE)
longtest <- melt_roc(test, "D", c("M1", "M2"))

ggplot(longtest, aes(d = D, m = M, color = name)) + geom_roc() + style_roc()

------
  
library(ggplot2)
library(plotROC)
ROCR1 <- read.table("CP.tsv", sep="\t", header=TRUE)

test2 <- data.frame(WT = ROCR1$PWTB, P301 = ROCR1$P301, P291 = ROCR1$P291)
longtest <- melt_roc(test, "WT", c("P301", "P291"))
head(longtest)
basicplot <- ggplot(longtest2, aes(d = D, m = M, color = name)) + geom_roc() + style_roc()


basicplot + 
  style_roc(theme = theme_grey) +
  theme(axis.text = element_text(colour = "blue")) +
  ggtitle("Themes and annotations") + 
  annotate("text", x = .75, y = .25, 
           label = paste("AUC =", round(calc_auc(basicplot)$AUC, 2))) +
  scale_x_continuous("1 - Specificity", breaks = seq(0, 1, by = .1))