## SAS/R inclass 2017.5.10 ##


setwd("C:/Users/asus/Documents/R/Data")
library(RSQLite)
library(maps)


f = read.csv("flights.csv",header=T,sep=",")
attach(f)


sqlite = dbDriver("SQLite")

flight = dbConnect(sqlite,"flights.db")
dbListTables(conn=flight)
dbListFields(conn=flight,name="SEA")

div = dbGetQuery(conn=flight, statement="select Diverted from SEA")

table(Diverted)

las = dbGetQuery(conn=flight, statement="select ArrDelay, DepDelay from SEA where Dest='LAS'")

dbGetQuery(conn=flight,
           statement="select avg(ArrDelay),avg(DepDelay) from SEA where Dest='Las'")

dbDisconnect(flight)

##
g = read.csv("locations.csv")
str(g)
g1=g2=g
names(g1) = c("Origin","o.lat","o.long")
names(g2) = c("Dest","d.lat","d.long")

h1 = merge(f,g1,by="Origin")
h2 = merge(h1,g2,by="Dest")

##
map("state")
attach(h2)

segments(-o.long,o.lat,-d.long,d.lat,col=4,lwd=0.5)


##inclass exercise
sort(table(Dest),decreasing=T);
plot(AirTime~Distance)



