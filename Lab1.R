 ############Problem 1#############
Auto=read.csv ("Auto.csv")
fix(Auto)


Auto=read.csv ("Auto.csv", header =T,na.strings ="?")
fix(Auto)
dim(Auto)
Auto=na.omit(Auto)
dim(Auto)
names(Auto)
head(Auto)

attach(Auto)
sapply(Auto, class)
summary(Auto)
Auto$origin <- factor(Auto$origin, levels=1:3, labels=c("U.S.", "Europe", "Japan"))
sapply(Auto, class)
quantitative <- sapply(Auto, is.numeric)
quantitative



x=c("mpg"=range(mpg),"cylinders"=range(cylinders),"displacement"=range(displacement)
,"horsepower"=range(horsepower)
,"weight"=range(weight)
,"acceleration"=range(acceleration)
,"year"=range(Auto[,7]))

summary(Auto)

mpgms=c(mean=mean(mpg), standarddeviation=sd(mpg))
cylindersms=c(mean=mean(cylinders), standarddeviation=sd(cylinders))
displacementms=c(mean=mean(displacement), standarddeviation=sd(displacement))
horsepowerms=c(mean=mean(horsepower), standarddeviation=sd(horsepower))
weightms= c(mean=mean(weight), standarddeviation=sd(weight))
accelarationms=c(mean=mean(acceleration), standarddeviation=sd(acceleration))
yearms= c(mean=mean(Auto[,7]), standarddeviation=sd(Auto[,7]))

MeanandStad=c(mpgms,cylindersms,displacementms,horsepowerms,weightms,accelarationms,yearms)
MeanandStad

newmsd <- sapply(Auto[-10:-85, quantitative], function(x) round(c(range(x), mean(x), sd(x)), 2))
rownames(newmsd) <- c("min", "max", "mean", "sd")
newmsd

pairs(Auto)
plot(Auto$mpg ,Auto$displacement)
plot(Auto$mpg ,Auto$horsepower)
plot(Auto$horsepower ,Auto$displacement)
#############Problem 2#########
require(ISLR) 
  data(Wage)
    head(Wage)
  plot(Wage$age, Wage$wage)
levels(Wage$education)

 gp1 = (Wage$education == "1. < HS Grad")
       points(Wage[gp1,2], Wage[gp1,11], pch = 16, col="darkgreen")
	   
gp2 = (Wage$education ==  "2. HS Grad")
points(Wage[gp2,2], Wage[gp2,11], pch = 15, col="red")

gp3 = (Wage$education ==  "3. Some College")
 points(Wage[gp3,2], Wage[gp3,11], pch = 14, col="blue")

gp4 = (Wage$education ==  "4. College Grad" )
 points(Wage[gp4,2], Wage[gp4,11], pch = 13, col="pink")

gp5 = (Wage$education == "5. Advanced Degree")
 points(Wage[gp5,2], Wage[gp5,11], pch = 12, col="orange")




########Problem 3#########
mytable=table(Wage$jobclass, Wage$education)
ftable(mytable)

#####Problem 4#########
X=rnorm(100)
Y=rnorm(100)
var1= 2*X+Y
var2= 2*X-Y
corr=cor(var1,var2)
corr
###########2nd Part###
nrep=50
for(i in 1:nrep){
X=rnorm(100)
Y=rnorm(100)
var1= 2*X+Y
var2= 2*X-Y
corl[i]=cor(var1,var2)
}
plot(corl)