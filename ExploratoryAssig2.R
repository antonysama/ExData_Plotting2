Code<-readRDS("Code2.rds")
pm25<-readRDS("PM25(2).rds")
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
     #Q4
     #subset df contating Comb & Coal
     Code2<-Code[grep("Comb(.*)Coal", Code$EI.Sector),]
     SCC2<-Code2[,1]
     #after merging check that no. observations are <pm25
     m2<-merge(Code2,pm25, by.x="SCC", by.y="SCC")
     #get what we need
     m3<-m2[,c(18:20)]
     m4<-m3[,c(1,3)]
     #by year, Emissions
     library("dplyr")
     m5<-m4%>%
       group_by(year)%>%
       summarise_each(funs(sum))
     #ggplot by year
     library("ggplot2")
     qplot(year, Emissions, data=m5)
      #Q5
     Baltimore & On-road
     q2<-pm25[pm25$fips=="24510" & pm25$type=="ON-ROAD",]
     #take yEAR and emmissions
     q3<-q2[,c(4,6)]
     #by year, Emissions
     library("dplyr")
     q3<-q3%>%
       group_by(year)%>%
       summarise_each(funs(sum))
     #ggplot by year
     library("ggplot2")
     qplot(year, Emissions, data=q3)
     #Q6 Baltimore & LA county, on-road
     r<-pm25[pm25$fips==c("24510","06037") & pm25$type=="ON-ROAD",]
     #take yEAR , emmissions & fips
     r2<-r[,c(1,4,6)]
     #by fips, fyear, Emissions
     r2<-r2%>%
       group_by(fips,year)%>%
       summarise_each(funs(sum))
     #ggplot by year
     library("ggplot2")
     qplot(year, Emissions, data=r2, facets=.~fips)
