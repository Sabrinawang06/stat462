
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
bestglm(Xy,IC="AIC",family=binomial(link="logit"))

logistic_best<-glm(quality_logi~volatile.acidity+residual.sugar+chlorides+free.sulfur.dioxide+sulphates+alcohol+type,family=binomial(link="logit"))
summary(logistic_best)
par(mfrow=c(2,2))
plot(logistic_best)


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

###USE this part ####
#####Logistic model#####
attach(wine)
#create dummy variable
dummy=vector("numeric",nrow(wine))
dummy[wine$type=='red']=1
dummy[wine$type=='white']=0

summary(wine$quality)

# 3-5 are lower quality (0), 6-8 are upper quality (1)
quality_logi<-vector('numeric',1000)
quality_logi[wine$quality<=5]=0
quality_logi[wine$quality>5]=1

######using scaled x's

scaled.wine = scale(wine[c("fixed.acidity","volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide","total.sulfur.dioxide","pH","sulphates","alcohol")])
scaled.wine<-cbind(scaled.wine,dummy)
summary(scaled.wine)

scaled_logistic=glm(quality_logi~scaled.wine,family=binomial(link="logit"))
summary(scaled_logistic)

##bestglm selection
require(bestglm)
Xy2<-as.data.frame(cbind(scaled.wine,quality_logi))
bestglm(Xy2,IC="AIC",family=binomial(link="logit"))

scaled.wine<-as.data.frame(scaled.wine)
best_logistic<-glm(quality_logi~volatile.acidity+residual.sugar+chlorides+free.sulfur.dioxide+sulphates+alcohol+dummy,data=scaled.wine,family=binomial(link="logit"))
summary(best_logistic)
1-best_logistic$deviance/best_logistic$null.deviance # "R-squared"

plot(best_logistic$residuls,main="Observation")

##Change qulity into ordered factor
quality_order<-as.ordered(quality)

#####Ordinal logsitic
require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
require(ordinal)


scaled.wine<-as.matrix(scaled.wine)
ordinal_fit<-polr(quality_order~scaled.wine,Hess=TRUE)
summary(ordinal_fit)


ctable <- coef(summary(ordinal_fit))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
ctable <- cbind(ctable, "p value" = p)
ctable

scaled.wine<-as.data.frame(scaled.wine)
best_ordinal<-clm(quality_order~volatile.acidity+residual.sugar+sulphates+alcohol, data=scaled.wine)
summary(best_ordinal)


##Anova for quality and type
require(ggplot2)

anova_analysis<-aov(quality~type)
summary(anova_analysis)
ggplot(wine, aes(x = quality, fill = type)) + 
  geom_histogram(col = "black", alpha = 0.5, 
                 position = "identity") +
  scale_fill_discrete(name = "Quality Distribution for Different Type", 
                      breaks=c("A","B"), 
                      labels = c("1","2")) # plot

which(chlorides>0.6)


acidity<-seq(5,20,0.5)
xbeta <- acidity*0.879
logistic_cdf <- function(x) {
  return( 1/(1+exp(-x) ) )
}

p1 <- logistic_cdf( 3.9381 - xbeta )
p2 <- logistic_cdf( 5.8492 - xbeta ) - logistic_cdf( 3.9381 - xbeta )
p3 <- logistic_cdf( 9.0082 - xbeta ) - logistic_cdf( 5.8492 - xbeta )
p4 <- logistic_cdf( 11.5812 - xbeta ) - logistic_cdf(  9.0082- xbeta )
p6 <- logistic_cdf( 13.7805 - xbeta ) - logistic_cdf( 11.5812 - xbeta )
p8 <- 1 - logistic_cdf( 13.7805 - xbeta )

plot(acidity,p1, type='l', ylab='Probability',xlab='Alcohol Content', ylim=c(0,1),main="Probability of Quality vs. Alcohol Concent")
lines(acidity,  p2, col='red')
lines(acidity,  p3, col='blue')
lines(acidity,  p4, col='green')
lines(acidity,  p6, col='purple')
lines(acidity,  p8, col='brown')
abline(v=14.05, lty=2, col="black")
abline(v=8, lty=2, col="black")
legend("topleft", lty=1, col=c("black", "red", "blue", "green", "purple", "brown"), 
       legend=c("3", "4", "5", "6", "7", "8"), title = 'Quality Level')

acidity<-seq(5,20,0.5)
xbeta <- acidity*3.419
logistic_cdf <- function(x) {
  return( 1/(1+exp(-x) ) )
}

p1 <- logistic_cdf( 3.9381 - xbeta )
p2 <- logistic_cdf( 5.8492 - xbeta ) - logistic_cdf( 3.9381 - xbeta )
p3 <- logistic_cdf( 9.0082 - xbeta ) - logistic_cdf( 5.8492 - xbeta )
p4 <- logistic_cdf( 11.5812 - xbeta ) - logistic_cdf(  9.0082- xbeta )
p6 <- logistic_cdf( 13.7805 - xbeta ) - logistic_cdf( 11.5812 - xbeta )
p8 <- 1 - logistic_cdf( 13.7805 - xbeta )

plot(acidity,p1, type='l', ylab='Probability',xlab='Volatile Acidity', ylim=c(0,1),main="Probability of Quality vs. Alcohol Concent")
lines(acidity,  p2, col='red')
lines(acidity,  p3, col='blue')
lines(acidity,  p4, col='green')
lines(acidity,  p6, col='purple')
lines(acidity,  p8, col='brown')
abline(v=14.05, lty=2, col="black")
abline(v=8, lty=2, col="black")
legend("topleft", lty=1, col=c("black", "red", "blue", "green", "purple", "brown"), 
       legend=c("3", "4", "5", "6", "7", "8"), title = 'Quality Level')


