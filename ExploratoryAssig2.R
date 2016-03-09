Code<-readRDS("Code.rds")
pm25<-readRDS("PM25.rds")
pm25<-pm25[,c(4,6)]
str(pm25)
#Q1library("reshape2")
m<-melt(pm25,id="Emissions")
d<-dcast(m,Emissions, year, sum)
library("dplyr")
e<-pm25%>%
  group_by(year)%>%
  summarise_each(funs(sum, mean))
with(e, plot(year, sum)
#Q2Baltimore
q2<-pm25[pm25$fips=="24510",]
q2<-q2[,c(4,6)]
library("dplyr")
q2<-q2%>%
  group_by(year)%>%
  summarise_each(funs(sum, mean))
with(q2, plot(year, sum))
#Q3

library("ggplot2")
qplot(Emissions, year, data=pm25, facets=.~type)
     