wc.at <- read.csv("bottle.csv",header=T)
View(wc.at)

#wc.at <- read.csv("C:/Users/HP/Santosh/ Linear Regression/Data Sets/wc-at.csv",header=T)
#getwd()
#View(wc.at)

install.packages("lattice")
library("lattice")
?lattice

# Exploratory data analysis
summary(wc.at)
#aggregate(wc.at)

?dotplot

# Graphical exploration
#dotplot(wc.at$Waist, main="Dot Plot of Waist Circumferences")
#dotplot(wc.at$AT, main="Dot Plot of Adipose Tissue Areas")
boxplot(wc.at$Salnty,col="yellow")
boxplot(wc.at$T_degC,col="red", horizontal = T)
?boxplot

hist(wc.at$Salnty)
hist(wc.at$T_degC)

qqnorm(wc.at$Salnty)
qqline(wc.at$Salnty)

qqnorm(wc.at$T_degC)
qqline(wc.at$T_degC)

hist(wc.at$Salnty, prob=TRUE)            # prob=TRUE for probabilities not counts
#lines(density(wc.at$Waist))             # add a density estimate with defaults
#lines(density(wc.at$Waist, adjust=2), lty="dotted")   # add another "smoother" density

hist(wc.at$T_degC, prob=TRUE)            # prob=TRUE for probabilities not counts
#lines(density(wc.at$AT))             # add a density estimate with defaults
#lines(density(wc.at$AT, adjust=4), lty="dotted")   # add another "smoother" density

#Scatter plot
plot(wc.at$Salnty,wc.at$T_degC,main="Scatter Plot", col="yellow", 
     col.main="red", col.lab="green", xlab="Temprature of Days", 
     ylab="Adipose Tissue area", pch=20)  # plot(x,y)

?plot
## alternate simple command
plot(wc.at$Salnty,wc.at$T_degC)

attach(wc.at)

cor(Salnty, T_degC)



reg <- lm(T_degC~Salnty, data=wc.at) # Y ~ X
summary(reg)


confint(reg,level=0.95)
?confint
?predict

pred <- predict(reg,interval="predict")

pred <- as.data.frame(pred)

pred
View(pred)
?predict
 
cor(pred$fit, wc.at$T_degC)

# transform the variables to check whether the predicted values are better
reg_sqrt <- lm(T_degC~sqrt(Waist), data=wc.at)
summary(reg_sqrt)

confint(reg_sqrt,level=0.95)
predict(reg_sqrt,interval="predict")
pred1 <- predict(reg_sqrt,interval="predict")
pred1
pred1 <- as.data.frame(pred1)
cor(pred1$fit, wc.at$T_degC)

reg_log<-lm(T_degC~log(Salnty), data=wc.at)
summary(reg_log)

confint(reg_log,level=0.95)
predict(reg_log,interval="predict")
pred2 <- predict(reg_log,interval="predict")
pred2 <- as.data.frame(pred2)
cor(pred2$fit, wc.at$T_degC) 

reg1<-lm(log(T_degC)~Salnty + I(Salnty*Salnty), data=wc.at)
summary(reg1)
confint(reg1,level=0.95)
predict(reg1,interval="predict")
pred<-predict(reg1,interval="predict")
?predict


pred<-as.data.frame(pred)
View(pred)
?exp

exp(pred$fit)

cor(exp(pred$fit),wc.at$T_degC)

reg_sqrt1<-lm(sqrt(T_degC)~Salnty, data=wc.at)
summary(reg_sqrt1)
confint(reg_sqrt1,level=0.95)
predict(reg_sqrt1,interval="predict")

reg_log1<-lm(log(T_degC)~Salnty, data=wc.at)
summary(reg_log1)
confint(reg_log1,level=0.95)
predict(reg_log1,interval="predict")

