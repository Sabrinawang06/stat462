wine<-read.csv("wine_sample.csv", header=TRUE)

# Looking at scaling data

# create dummy
n = nrow(wine)
dummy=vector("numeric",n)
dummy[wine$type=='red']=1
dummy[wine$type=='white']=0

model = lm(wine$quality~volatile.acidity+residual.sugar+chlorides+free.sulfur.dioxide+sulphates+alcohol+dummy, data=wine)

summary(model)
# We can see that the Betas are very different and their scales are not similar, making it tough to compare them

head(wine)

scaled.wine = scale(wine[c("volatile.acidity","residual.sugar","chlorides","free.sulfur.dioxide","sulphates","alcohol")])
summary(scaled.wine)

scaled_model = lm(wine$quality ~ scaled.wine)
summary(scaled_model)
# Now the Betas are much more similar due to the scaling


# Looking at influential and leverage points

residuals = scaled_model$residuals
sigma_hat = summary(scaled_model)$sigma
X = model.matrix(wine$quality ~ scaled.wine)
H = X%*%solve(t(X)%*%X)%*%t(X)
h = diag(H)
h #leverages
r = residuals/(sigma_hat*sqrt(1-h))

p=5
sum(h)

# The two threshholds are 2*(p/n) and 3*(p/n)
thresh2=2*p/n
thresh3=3*p/n

plot(h,xlab="Obs #", ylab="Leverage", main="Leverage")
abline(h=thresh2,lty=2,col="red")
abline(h=thresh3,lty=2,col="blue")

wine$color="black"
wine$color[h>=thresh2]="red"

r = residuals/(sigma_hat*sqrt(1-h))
plot(r, xlab="Obs #", ylab="Standardized Residuals", main="Standardize Residuals", col=wine$color)

t = r*sqrt((n-p-1)/(n-p-r^2))
wine$color[t>1.7]="green"
plot(t,xlab='Observation #',ylab='Studentized residuals',main='Studentized residuals', col=wine$color)

cook=(1/p)*r^2*h/(1-h)
plot(cook,xlab='Observation #',ylab='Cook\'s distance',main='Cook\'s distance', col=wine$color)
