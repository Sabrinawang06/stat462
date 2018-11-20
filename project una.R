#read in the data
wine<-read.csv("wine_sample.csv", header=TRUE)
wine$type
summary(wine)
n=nrow(wine)
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

#There is a problem with this part. I can only comput 8 RSS but my p is 12.
install.packages('leaps')
require(leaps)
subset1=regsubsets(quality~. -X-type-density+dummy,nbest = 1,method = 'exhaustive',data=wine,nvmax = 13)
sum_subset<-summary(subset1)
sum_subset$which
#compute R2_adj
p_full=12
p=2:p_full
RSS_p=sum_subset$rss
totalSS=sum((wine$quality)-mean(wine$quality)^2)

n=nrow(wine)
R2_adj=1-(RSS_p/(n-p))/(totalSS/(n-1))
R2_adj
min(R2_adj)
#smallest adjusted R^2 when p=8
#Cp
sigma_hat_full=summary(linear_full)$sigma
C_p=RSS_p/(sigma_hat_full^2)+2*p-n
C_p
par(mfrow=c(1,1))
plot(p,C_p,xlab="Number of betas",ylab="Mallow's Cp")
abline(0,1)
#when p=7 

#aic
aic_p=n*log(RSS_p/n)+2*p
aic_p
min(aic_p)
plot(p,aic_p,xlab="Number of betas",ylab="AIC")
#when p=7

#bic
bic_p=n*log(RSS_p/n)+p*log(n)
bic_p
min(bic_p)
plot(p,bic_p,xlab="Number of betas",ylab="BIC")
#when p=4
 
#we should use p=7 with volatile.acidity,residual.sugar,chlorides,free.sulfur.dioxide,sulphates,alcohol,dummy

linear_red<-lm(quality~volatile.acidity+residual.sugar+chlorides+free.sulfur.dioxide+sulphates+alcohol+dummy,data = wine)
summary(linear_red)

#p=7 might not be a good model since R^2 is 0.2627 and we have free.sulfur.dioxide insignificant.
# we might have collinearity with free.sulfur.dioxide
