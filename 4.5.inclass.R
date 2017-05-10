## SAS/R in class 2017.4.5


str(iris);
attach(iris);
plot(Sepal.Length,Sepal.Width,type = "n");
points(Sepal.Length[Species!="setosa"],Sepal.Width[Species !="setosa"],col="red");
points(Sepal.Length[Species=="setosa"],Sepal.Width[Species =="setosa"],col="blue",pch=2);
legend("bottomright",c("setosa","not setosa"),pch=c(1,2),col=c("red","blue"));


isSetosa = (Species == "setosa");
isSetosa[isSetosa == T]= 1;
isSetosa[isSetosa ==F] = 0;
g= glm(isSetosa~Sepal.Length+Sepal.Width, data=iris, maxit=30,family = "binomial");

##maxit = maximum iterations 最大疊帶次數


##round 取小數點幾位
round(g$coef,2);


x= read.csv("UCI_Credit_Card.csv");

idx0 = which(x[,25]==0);
idx1 = which(x[,25]==1);
set.seed(250);

testid= c(sample(idx0,1000),sample(idx1,500));
train = x[-testid,];
test = x[testid,];

g =glm(default.payment.next.month~. , data=train, maxit=50, family="binomial");
pred.g = predict(g,test[,-25],type="response");
length(pred.g);

table(round(pred.g),x[testid,25],dnn=c("predict","actual"));

#######HW3#####


##Q1
str(x)
m = barplot(table(x$MARRIAGE),col="dark green", main = "Marriage",beside = T)
legend("topright",legend = c("non","married","single","others"),,pch=c("0","1","2","3"))
table(x$default.payment.next.month,x$MARRIAGE,dnn=c("payment","Marriage"))

#Q3
##convert to factor
for(i in c(3,4,5,7:12)){
  x[,i]= as.factor(x[,i]);
}

idx0 = which(x[,25]==0);
idx1 = which(x[,25]==1);
set.seed(250);
testid= c(sample(idx0,1000),sample(idx1,500));
train = x[-testid,];
test = x[testid,];
g2 =glm(default.payment.next.month~. , data=train, maxit=50, family="binomial");
pred.g2 = predict(g2,test[,-25],type="response");
table(round(pred.g2),x[testid,25],dnn=c("predict","actual"));
###############
library(nnet)

m= matrix(1:8 ,nc=2)



####
set.seed(200);
testidx = sample(150,30);
iristrain = iris[-testidx,];
iristest= iris[testidx,];
g2 = multinom(Species~., data=iristrain);
summary(g2);

predict(g2, iristest,"probs")
round(predict(g2,iristest,"probs"),1)
pred.g2 = predict(g2,iristest,"probs");


test.table = function(true,pred){
  pred = max.col(pred);
  table(true,pred);
  
}
class.iris = c(rep(1,50),rep(2,50),rep(3,50));
test.table(class.iris[testidx],pred.g2);

##mm = max.col(pred.g2)
##a = c(1,3,5,5,7,3,4,5)
##b=c(3,3,2,5,4,3,4,5)

apply(pred.g2,1,sum);

###set.seed(100)
set.seed(100);
testidx = sample(150,30);
iristrain = iris[-testidx,];
iristest= iris[testidx,];
g2 = multinom(Species~., data=iristrain);
summary(g2);

predict(g2, iristest,"probs")
round(predict(g2,iristest,"probs"),1)
pred.g2 = predict(g2,iristest,"probs");


test.table = function(true,pred){
  pred = max.col(pred);
  table(true,pred);
  
}
class.iris = c(rep(1,50),rep(2,50),rep(3,50));
test.table(class.iris[testidx],pred.g2);

###
###set.seed(300)
set.seed(300);
testidx = sample(150,30);
iristrain = iris[-testidx,];
iristest= iris[testidx,];
g2 = multinom(Species~., data=iristrain);
summary(g2);

predict(g2, iristest,"probs")
round(predict(g2,iristest,"probs"),1)
pred.g2 = predict(g2,iristest,"probs");


test.table = function(true,pred){
  pred = max.col(pred);
  table(true,pred);
  
}
class.iris = c(rep(1,50),rep(2,50),rep(3,50));
test.table(class.iris[testidx],pred.g2);

