apple = read.table("Applewood_new.txt",header=T);
attach(apple);
Previous=as.factor(Previous);
apple[,5]=Previous;
reg1=lm(Profit~. , data=apple);
reg2=lm(Profit~Age,data=apple);

## Q1

origin = apple[Age==20 | Age==25 | Age==30,];
select = which(apple$Age==20 | apple$Age==25 | apple$Age==30);
fitted = reg2$fitted.values[select];
cbind(origin,fitted);

newdata=data.frame(Age=c(20,25,35))
prdictted = predict(reg2,newdata)
predictted= cbind(Age=c(20,25,30),prdictted)


predictted = predict(reg2,data.frame(Age=apple$Age))
fitted = reg2$fitted.values[]
cbind(origin,fitted,predictted)

##Q2

attach(mtcars);
reg3 = lm(mpg~wt,data=mtcars);
plot(mpg~wt);
abline(reg3,col="red")

##Q3
reg4=lm(mpg~wt+factor(am),data=mtcars);
table=summary(reg4)$coefficients;






