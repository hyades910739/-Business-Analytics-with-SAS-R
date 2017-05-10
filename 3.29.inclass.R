reg3 = lm(mpg~ . ,data=mtcars);


reg4 = lm(mpg~ wt+qsec+am);
summary(reg4);

boxplot(mtcars$mpg~mtcars$am,xlab="Transmisson type",ylab="Miles per gallon",main="mtcars");

pairs(mtcars[,c(1,6,7,8)])


#####################

library(glmnet);
set.seed(20);
x= matrix(rnorm(200*150),200,150);
y=rnorm(200);
test.x = matrix(rnorm(200*150),200,150);
test.y= rnorm(200);

fit = lm(y~x);
fit1 = glmnet(x,y);
fit2 = glmnet(x,y,alpha=0);
cv1= cv.glmnet(x,y);
cv2= cv.glmnet(x,y,alpha=0);

##test
pred = predict(fit,newx = test.x);
pred1 = predict(cv1,newx = test.x, s="lambda.min");
pred2 = predict(cv2, newx = test.x, s="lambda.min");
mse = mean((test.y-pred)^2);
mse1 = mean((test.y-pred1)^2);
mse2 = mean((test.y-pred2)^2);
c(mse,mse1,mse2);

##train
pred.tr = predict(fit,newx = x);
pred1.tr = predict(cv1,newx = x, s="lambda.min");
pred2.tr = predict(cv2, newx = x, s="lambda.min");
mse.tr = mean((y-pred.tr)^2);
mse1.tr = mean((y-pred1.tr)^2);
mse2.tr = mean((y-pred2.tr)^2);
c(mse.tr,mse1.tr,mse2.tr);



