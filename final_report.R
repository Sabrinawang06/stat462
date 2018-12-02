
##Final Report

#read in the data
wine<-read.csv("wine_sample.csv", header=TRUE)
summary(wine[,-1])

#####Logistic model#####
summary(wine$quality)

# 3-5 are lower quality (0), 6-8 are upper quality (1)
quality_logi<-vector('numeric',1000)
quality_logi[wine$quality<=5]=0
quality_logi[wine$quality>5]=1

logistic_fit=glm(quality_logi~.-X-quality,data=wine,family=binomial(link="logit"))
summary(logistic_fit)
par(mfrow=c(2,2))
plot(logistic_fit)

reduced_model<-update(logistic_fit, .~.-citric.acid)
summary(reduced_model)
reduced_model2<-update(reduced_model, .~.-total.sulfur.dioxide)
summary(reduced_model2)
reduced_model3<-update(reduced_model2, .~.-pH)
summary(reduced_model3)
reduced_model4<-update(reduced_model3, .~.-fixed.acidity)
summary(reduced_model4)
reduced_model5<-update(reduced_model4, .~.-free.sulfur.dioxide)
summary(reduced_model5)


##Final Report

#read in the data
wine<-read.csv("wine_sample.csv", header=TRUE)
summary(wine[,-1])
attach(wine)

###check vif
library(xtable)
cor<-round(cor(wine[-c(1,13,14)]),2)
upper<-cor
upper[upper.tri(cor)]<-""
upper<-as.data.frame(upper)
table<-xtable(upper)
print(table, typle="html",comment=FALSE,scalebox = 0.7)

#density has -0.7 with alcohol
library(car)
plot(density~alcohol)
vif(lm(quality~.-X-quality, data=wine)) #remove density
vif<-vif(lm(quality~.-X-density-quality, data=wine))

vif<-as.data.frame(vif)

vif1<-vif(lm(quality~.-X-quality, data=wine)) #remove density
vif1<-as.data.frame(vif1)
vif1<-xtable(vif1,auto=TRUE)
print(vif1,type="latex",comment=FALSE)
#####Regular Regression model


#####Logistic model#####
summary(wine$quality)

# 3-5 are lower quality (0), 6-8 are upper quality (1)
quality_logi<-vector('numeric',1000)
quality_logi[wine$quality<=5]=0
quality_logi[wine$quality>5]=1

logistic_fit=glm(quality_logi~.-X-density-quality,data=wine,family=binomial(link="logit"))
summary(logistic_fit)
par(mfrow=c(2,2))
plot(logistic_fit)

####alpha-to-remove method
reduced_model<-update(logistic_fit, .~.-citric.acid)
summary(reduced_model)
reduced_model2<-update(reduced_model, .~.-total.sulfur.dioxide)
summary(reduced_model2)
reduced_model3<-update(reduced_model2, .~.-pH)
summary(reduced_model3)
reduced_model4<-update(reduced_model3, .~.-fixed.acidity)
summary(reduced_model4)
reduced_model5<-update(reduced_model4, .~.-free.sulfur.dioxide)
summary(reduced_model5)

####Use Bestglm
require(bestglm)
Xy<-cbind(wine[,-c(1,9,13)],quality_logi)
bestglm(Xy,IC="AIC")


#####Ordinal logsitic
require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
quality_factor<-as.factor(wine$quality)
ordinal_fit<-polr(quality_factor~.-X-density-quality,data=wine, Hess=TRUE)
summary(ordinal_fit)


ctable <- coef(summary(ordinal_fit))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
ctable <- cbind(ctable, "p value" = p)
ctable


