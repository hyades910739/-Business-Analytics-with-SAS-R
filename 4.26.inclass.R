##### SAS/R inclass 2017.4.26 #####

library(MASS);

gr= lm.ridge(Employed~. , 
             longley,lambda=seq(0.0,0.1,0.001));

matplot(gr$lambda,t(gr$coef),type="l",
        xlab=expression(lambda),ylab=expression(hat(beta)))

## control coef by lambda, when lambda increase, beta.hat gets lower 
##(bigger coef means the model is more complex)


abline(h=0,lwd=2);

g = lm.ridge(Employed~., longley);
round(g$coef,2)


##
set.seed(200);
testidx= sample(150,30);
iristrain = iris[-testidx,];
iristest = iris[testidx,];

library(glmnet);
cv.fit = cv.glmnet(as.matrix(iristrain[,-5]),iristrain[,5],
                   alpha=0.8,family="multinomial");
prediction = predict(cv.fit, newx = as.matrix(iristest[,-5]),type="class")

##pred =  max.col(prediction[,,1]);
##pred[pred==1] = "setosa";
##pred[pred==2] = "versicolor";
##pred[pred==3] = "virginica";
##table(pred,iristest$Species);
table(prediction,iristest$Species);



iris.sub = iris[1:100,];
set.seed(200);
testidx = c(sample(1:50,10),sample(51:100,10));
iristrain = iris.sub[-testidx,];
iristest = iris.sub[testidx,];
g =glm(Species~.,data=iristrain,family = "binomial");

pred.g = predict(g,iristest);
roc.g = roc(iristest[,5],pred.g);
plot(roc.g);
auc(roc.g);


##

x = read.csv("UCI_Credit_Card.csv");
idx0 = which(x[,25]==0);
idx1 = which(x[,25]==1);
set.seed(250);
testid = c(sample(idx0,1000),sample(idx1,500));
train = x[-testid,]
test = x[testid,]

g = glm(default.payment.next.month~., data=train, maxit=50,family="binomial");

##
pred.g = predict(g,test[,-25],type="response");
roc.g = roc(x[testid,25],pred.g);
plot(roc.g)
auc(roc.g)


