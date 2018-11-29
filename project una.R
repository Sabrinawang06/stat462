#read in the data
wine<-read.csv("wine_sample.csv", header=TRUE)
wine$type
summary(wine)
n=nrow(wine)
#compute VIF
X=model.matrix(wine$quality~wine$fixed.acidity+wine$volatile.acidity+wine$citric.acid+wine$residual.sugar+wine$chlorides+wine$free.sulfur.dioxide+wine$total.sulfur.dioxide+wine$density+wine$pH+wine$sulphates+wine$alcohol)
R2=vector("numeric",11)
for(j in 1:11){
  y_tmp=X[,1+j]
  x_tmp=as.matrix(X[,-c(1,1+j)])
  lm_fit=lm(y_tmp~x_tmp)
  R2[j]=summary(lm_fit)$r.squared
  }
VIF=1/(1-R2)
names(VIF)=c('fixed.acidity','volatile.acidity','citric.acid','residual.sugar','chlorides','free.sulfur.dioxide','total.sulfur.dioxide','density','pH','sulphates','alcohol')
VIF
#drop density and re-compute
X2=model.matrix(wine$quality~wine$fixed.acidity+wine$volatile.acidity+wine$citric.acid+wine$residual.sugar+wine$chlorides+wine$free.sulfur.dioxide+wine$total.sulfur.dioxide+wine$pH+wine$sulphates+wine$alcohol)
R2_2=vector("numeric",10)
for(j in 1:10){
  y_tmp2=X2[,1+j]
  x_tmp2=as.matrix(X2[,-c(1,1+j)])
  lm_fit2=lm(y_tmp2~x_tmp2)
  R2_2[j]=summary(lm_fit2)$r.squared
}
VIF2=1/(1-R2_2)
names(VIF2)=c('fixed.acidity','volatile.acidity','citric.acid','residual.sugar','chlorides','free.sulfur.dioxide','total.sulfur.dioxide','pH','sulphates','alcohol')
VIF2
#create dummy variable
dummy=vector("numeric",n)
dummy[wine$type=='red']=1
dummy[wine$type=='white']=0
#take out density due to collinearity 
linear_full <- lm(quality~. -X-type-density+dummy,data = wine)
summary(linear_full)
plot(linear_full)
#select model
#backward selection
linear_red1 <- lm(quality~. -X-type-density+dummy-citric.acid,data = wine)
summary(linear_red1)
linear_red2 <- lm(quality~. -X-type-density+dummy-citric.acid-pH,data = wine)
summary(linear_red2)
linear_red3 <- lm(quality~. -X-type-density+dummy-citric.acid-pH-total.sulfur.dioxide,data = wine)
summary(linear_red3)
linear_red4 <- lm(quality~. -X-type-density+dummy-citric.acid-pH-total.sulfur.dioxide-fixed.acidity,data = wine)
summary(linear_red4)
linear_red5 <- lm(quality~. -X-type-density+dummy-citric.acid-pH-total.sulfur.dioxide-fixed.acidity-free.sulfur.dioxide,data = wine)
summary(linear_red5)
#We end up with 6 predictors.

install.packages('leaps')
require(leaps)
subset1=regsubsets(quality~. -X-type-density+dummy,nbest = 1,method = 'exhaustive',data=wine,nvmax = 13)
sum_subset<-summary(subset1)
sum_subset$which
#compute R2_adj
p_full=12
p=2:p_full
RSS_p=sum_subset$rss
totalSS=sum((wine$quality-mean(wine$quality))^2)

n=nrow(wine)
R2_adj=1-(RSS_p/(n-p))/(totalSS/(n-1))
R2_adj
plot(p,R2_adj,xlab="Number of betas",ylab="Adjusted R-squared")
max(R2_adj)
#max adjusted R^2 with 8 predictors
#Cp
sigma_hat_full=summary(linear_full)$sigma
C_p=RSS_p/(sigma_hat_full^2)+2*p-n
C_p
min(C_p)
par(mfrow=c(1,1))
plot(p,C_p,xlab="Number of betas",ylab="Mallow's Cp")
abline(0,1)
#when p=7 , with 6 predictors,

#aic
aic_p=n*log(RSS_p/n)+2*p
aic_p
min(aic_p)
plot(p,aic_p,xlab="Number of betas",ylab="AIC")
#with 8 predictors.

#bic
bic_p=n*log(RSS_p/n)+p*log(n)
bic_p
min(bic_p)
plot(p,bic_p,xlab="Number of betas",ylab="BIC")
# 5 predictors.
 
#we should use p=8 with volatile.acidity,residual.sugar,chlorides,free.sulfur.dioxide,sulphates,alcohol,dummy

linear_red<-lm(quality~volatile.acidity+residual.sugar+chlorides+free.sulfur.dioxide+sulphates+alcohol+dummy,data = wine)
summary(linear_red)
plot(linear_red)
#p=7 might not be a good model since R^2 is 0.2627 and we have free.sulfur.dioxide insignificant. 
#However,this might be the result of we treat quality as quality when it is in fact a categorical variable. The full model has only R^2 = 0.264

