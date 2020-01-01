setwd("C:/Users/HP/Desktop")
claimants <- read.csv(file.choose())            # Choose the claimants Data set
View(claimants)

claimants <- na.omit(claimants)
View(claimants)
model <- glm(ATTORNEY~.,data=claimants[,-1],family = "binomial")

# Confusion matrix table 
prob <- predict(model,type=c("response"),claimants)
prob

confusion<-table(prob>0.5,claimants$ATTORNEY)
confusion

confusion# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 70.62

pred_values <- NULL
yes_no <- NULL
for (i in 1:1096)
{
  pred_values[i] <- ifelse(prob[i]>=0.5,1,0)
  yes_no[i] <- ifelse(prob[i]>=0.5,"yes","no")
}

claimants[,"prob"] <- prob
claimants[,"pred_values"] <- pred_values
claimants[,"yes_no"] <- yes_no

View(claimants[,c(2,8,9,10)])

# Accuracy 
acc <- table(claimants$ATTORNEY,pred_values)
Accuracy<-sum(diag(acc)/sum(acc))
Accuracy # 70.62

# ROC Curve 
library(ROCR)
rocrpred<-prediction(prob,claimants$ATTORNEY)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))

# More area under the ROC Curve better is the logistic regression model obtained
#install.packages("epiR")
#library(epiR)
#epi.2by2(confusion, method="cohort.count")
