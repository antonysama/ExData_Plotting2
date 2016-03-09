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
     q2<-pm25[pm25$fips=="24510",c(4:6)]
     q2<-q2[,c(4,6)]
     library("dplyr")
     q2<-q2%>%
       group_by(year,type)%>%
       summarise_each(funs(sum))
     with(q2, plot(year, Emissions))
     #Q3
     install.packages("ggplot2")
     library("ggplot2")
     qplot(year,Emissions, data=q2, facets=.~type, geom = c("point","smooth"))
     qplot(year, data=q2, facets=type~., binwidth=2)
