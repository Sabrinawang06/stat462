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

