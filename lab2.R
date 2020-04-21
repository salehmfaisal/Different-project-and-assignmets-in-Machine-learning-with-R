data=read.csv("anom1.txt")
data
x<-data[,1]
y<-data[,2]
lm1<-lm(y~x)
summary(lm1)
plot(x,y)
abline(lm1)


##############Ex.1

x<-cars$speed
y<-cars$dist

lm2=lm(y~x)
summary(lm2)
plot(x,y)
lm3=lm(x~y)
summary(lm3)
coef1=lm2$coef
coef2=lm3$coef
print(coef1)
print(coef2)
plot(lm2$fit, lm2$resid)
n=length(x)
mse=sum((lm2$resid)^2)/n
print(mse)

n1=length(y)
mse1=sum((lm3$resid)^2)/n1
print(mse1)


###############Ex.2

states=read.csv("states.csv",row.names=1)
head(states)
pairs(states)
lm4=lm(HS.Grad~Income+Illiteracy, data=states)
summary(lm4)


HS=states[,6]
X=states[,-6]
X=as.matrix(X)

lm4a=lm(HS~X)
summary(lm4a)

plot(lm4a)
states
##to delete the outliars
dat1=states[-17,]
dat2=dat1[-30,]
data=dat2[-32,]

HS1=data[,6]
X1=data[,-6]
X1=as.matrix(X1)

lmn=lm(HS1~X1)
summary(lmn)

################Ex.3
data=read.csv("anom1.txt")
head(data)
n=length(x)
nhalf=n%/%2
train=sample(1:n, nhalf,replace=F)
xtrain=x[train]
ytrain=y[train]
plot(x,y)
points(xtrain,ytrain, pch=16)
test=setdiff(1:n, train)

