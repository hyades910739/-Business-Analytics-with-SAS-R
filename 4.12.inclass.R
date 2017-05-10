## SAS/R inclass 2017.4.12

x = read.table("Mondo.txt");
x = as.matrix(x);
barplot(table(x));



movie= read.table("u.data");
names(movie)=c("uid","iid","rating","timestamp")
x = table(movie[,2])
a = x[x>500]
a

item=read.table("u.item",sep="|",quote="")
mm=read.table("clipboard",header=T,sep="",quote="")