### SAS/R inclass 2017.05.0.3 ###

###lm
g = lm(mpg~wt,data=mtcars)
newdata= data.frame(wt=seq(0.5,5.5,0.1))

predict(g,newdata)

pred.c = predict(g,newdata,interval = "confidence",level=0.9)
pred.p = predict(g,newdata,interval = "prediction",level=0.9)

matplot(newdata$wt, cbind(pred.c,pred.p[,-1]),
        lty=c(1,2,2,3,3),col=c(1,2,2,3,3),type="l",
        ylab="predicted y",main="mpg~wt data = mtcars")


###glm
iris.sub = iris[1:100,]
set.seed(200)

testid = c(sample(1:50,10),sample(51:100,10))
iristrain = iris.sub[-testid,]
iristest = iris.sub[testid, ]

g = glm(Species~. , data= iristrain, maxit=50, family="binomial")

predict(g,iristest)
predict(g,iristest,type="link")

pred.l = predict(g,iristest,type="link")
exp(pred.l)/(1+exp(pred.l))
predict(g,iristest,type="response")

##glmnet
set.seed(200);
testid = sample(150,30)
train = iris[-testid,]
test = iris[testid,]
library(glmnet)

cv.fit = cv.glmnet(as.matrix(train[,-5]), train[,5],
                   alpha=1.0, family="multinomial")

pred.c = predict(cv.fit,newx = as.matrix(test[,-5]),
                 type="class")

table(pred.c,test$Species)

pred.l = predict(cv.fit,newx = as.matrix(test[,-5])
                 ,type= "link")

pred.r = predict(cv.fit,newx = as.matrix(test[,-5]),
                 type= "response")

####flight

flight = read.csv("flights.csv",header=T,sep=",")
attach(flight)

f.in = which(Dest == "SEA");
f.out = which(Origin == "SEA");

#
boxplot(DepDelay~Origin, main="DepDelay~Origin")
d.idx = which(DepDelay>800)
flight[d.idx,1:6]

boxplot(DepDelay~Origin,main="DepDelay~Origin",
        data=flight[flight$Dest=="LAS"| flight$Dest=="LAX",])

boxplot(DepDelay~Dest,main="DepDelay~Dest",
        data = flight[flight$Dest %in% c("LAS","LAX"),])

hist(DepTime)
hist(ArrTime)
hist(AirTime)

coplot(ArrDelay~DepDelay | DepTime)
coplot(ArrDelay~DepDelay | ArrTime)
coplot(ArrDelay~DepDelay | AirTime)

##
delay = aggregate(flight[,c("ArrDelay","DepDelay","AirTime")],
                  list(Origin),mean,na.rm=T)

sort(delay[,1])


##
g = lm(ArrDelay~DepDelay, data=delay)
plot(ArrDelay~DepDelay, data=delay)
abline(g,col="red")

g$coefficients

g1 = lm(ArrDelay~DepDelay)
plot(ArrDelay~DepDelay)
abline(g1)

g1$coefficients


