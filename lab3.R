####Problem-1

require(MASS)
data(Pima.tr)
data(Pima.te)
pima1=merge(Pima.tr,Pima.te,all=TRUE)
head(pima1)
attach(pima1)
plot(pima1)
 logreg1 = glm(type ~ npreg+glu+ bp+skin+bmi+ped+pima1$age , family = binomial, data = pima1)
  summary(logreg1)
names(pima1)

# Getting the "truth table"/ confusion table 
   probs1 = predict(logreg1, type="response")   
   pred.dia = (probs1 > 0.5)     # classify all probs > 0.5 as "failure"
   t1 = table(type,pred.dia) 
   print(t1)   
##backward selection
logreg2 = glm(type ~ npreg+glu+ bp+bmi+ped+age , family = binomial, data = pima1)
  summary(logreg2)

logreg3 = glm(type ~ npreg+glu+bmi+ped+age , family = binomial, data = pima1)
  summary(logreg3)

logreg4 = glm(type ~ npreg+glu+bmi+ped , family = binomial, data = pima1)
  summary(logreg4)

###So logreg3 is better model as it contains smaller AIC. So,number of pregnancies,plasma glucose concentration in an oral glucose tolerance test. 
##body mass index ,diabetes pedigree function,age in years are significantly affect the occurrence of diabetes.

####b
attach(pima1)
lda1 = lda(type ~ npreg+glu+bmi+ped+pima1$age)
 plot(lda1, dimen = 2)
 
  p1 = predict(lda1)
  head(p1$post)

# "confusion table", with different cutoffs
   
     cutoff = 0.5
	 pred.class = (predict(lda1)$post[,1] < cutoff)
	 t1 = table(type, pred.class)
	 n = length(type)
	 mis.prob1 = 1 - sum(diag(t1))/n
	 
	 print(t1)
	 print(mis.prob1)
	 
    cutoff = 0.8
	 pred.class2 = (predict(lda1)$post[,1] < cutoff)
	 t2 = table(type, pred.class2)
	 mis.prob2 = 1 - sum(diag(t2))/n
   
   	 print(t2)
	 print(mis.prob2)

##c

cutoff = 0.73
	 pred.class3 = (predict(lda1)$post[,1] < cutoff)
	 t3 = table(type, pred.class3)
	 mis.prob3 = 1 - sum(diag(t3))/n
   
   	 print(t3)
	 print(mis.prob3)

#d
lda2 = lda(type ~ npreg+glu+ bp+skin+bmi+ped+pima1$age)
lda2$scaling
#e
 qda1 = qda(type ~ npreg+glu+ bp+skin+bmi+ped+pima1$age)
  #Getting the "truth table"/ confusion table 
  cutoff=0.73
   pred.class4 = (predict(qda1)$post[,1] < cutoff)
	 t4 = table(type, pred.class4)
	 mis.prob4 = 1 - sum(diag(t4))/n
   
   	 print(t4)
	 print(mis.prob4)

#############Problem-2
auto = read.csv("Auto.csv", na.strings="?")
Auto = na.omit(auto)
xx = as.matrix(Auto[,2:7])
y = 1/Auto$mpg
head(Auto)
install.packages("leaps")
require(leaps)

b <- regsubsets(xx,y , nbest=1, nvmax=6, method="exhaustive")
 summary(b)

summary(b)$bic 
       which.min(summary(b)$bic)
  summary(b)$rss
  summary(b)$adjr2
  summary(b)$cp


 b1 <- regsubsets(xx, y, nbest=1, nvmax=6, method="backward")
 summary(b1)
 
  summary(b1)$bic 
which.min(summary(b1)$bic)


lm1 = lm(y  ~ horsepower+weight+year, data = Auto)
  summary(lm1)
 lm2 = lm(y  ~ horsepower+weight+year+acceleration, data = Auto)
   anova(lm1, lm2)

####b
#Regularizations: ridge regression and LASSO 
install.packages("glmnet")
require(glmnet)
 cgrid =10^seq (3,-3, length =100)
xx = as.matrix(Auto[,2:7])
y = 1/Auto$mpg
 ridge.mod = glmnet(xx,y,alpha =0, lambda = cgrid)
coefs = coef(ridge.mod)
 dim(coefs)
plot(cgrid, coefs[2,], type="l", log="x", ylim=c(-0.1,0.1))     # smoothing parameter lambda = c will be on log scale
    lines(cgrid, coefs[3,], lty=2) 
	lines(cgrid, coefs[4,], lty=3) 
	lines(cgrid, coefs[5,], lty=4)
      lines(cgrid, coefs[6,], lty=5)
      lines(cgrid, coefs[7,], lty=6)
    lines(cgrid, cgrid*0,col="magenta")


  cv1 = cv.glmnet(xx,y,alpha=0) 
  
  plot(cv1)
  cv1$lambda.min  # these are results for Ridge Regression 

 cv1b = cv.glmnet(xx,y,alpha=0, lambda = cgrid) 
     plot(cv1b)    # since there are no serious problems with colinearity, lambda is small 
	 (best1 = cv1b$lambda.min)
      cv1b$glmnet.fit
out = glmnet(xx,y,alpha=0, lambda = cgrid)
	 predict(out,type="coefficients", s = best1)




# let's compare these to the "full model" 
     lm1b = lm(y~xx)
	 lm1b$coef     # values from Ridge Reg. are mostly smaller, but not dramatically different 

	 predict(out,type="coefficients", s = 0)  # this is close to lm1b (but not 100% the same ==> numerical errors?)
	 

## LASSO 
 
 lasso1 = glmnet(xx,y,alpha = 1, lambda = cgrid)  
   # alpha = 0 in this function corresponds to ridge regression, alpha = 1 corresponds to LASSO
 coefs = coef(lasso1)
 dim(coefs)
 
 plot(cgrid, coefs[2,], type="l", log="x", ylim=c(-0.1,0.1))     # smoothing parameter lambda = c will be on log scale
    lines(cgrid, coefs[3,], lty=2) 
	lines(cgrid, coefs[4,], lty=3)  
	lines(cgrid, coefs[5,], lty=4)
      lines(cgrid, coefs[6,], lty=5)
      lines(cgrid, coefs[7,], lty=6)

	lines(cgrid, cgrid*0,col="magenta")


cv2 = cv.glmnet(xx,y,alpha=1, lambda = cgrid)   # now for LASSO 
    plot(cv2)
    (best2 = cv2$lambda.min)
     out = glmnet(xx,y,alpha=1, lambda = cgrid)
	 predict(out,type="coefficients", s = best2)



#####Problem3

require(ISLR)
data(Weekly)
head(Weekly)
attach(Weekly)
xx = as.matrix(Weekly[,1:7])
y = (Weekly$Direction == "Up")
cv3 = cv.glmnet(xx,y,alpha=1 , family="binomial")
  plot(cv3)
(best3 = cv3$lambda.min)
   out = glmnet(xx,y,alpha=1)
   predict(out,type="coefficients", s = best3)
# Getting the "truth table"/ confusion table 
  probs=predict(out,type="response",newx=xx, s = best3)
   
   pred.dia = (probs > 0.54)   
   t1 = table(Direction,pred.dia) 
   print(t1) 
sum(diag(t1)/length(y))  


