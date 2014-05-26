plot3=function()
{
setwd("C:/Users/Home/Downloads/exdata-data-NEI_data")
nei=readRDS("summarySCC_PM25.rds")
scc=readRDS("Source_Classification_Code.rds")
dataBaltimore=nei[nei$fips=="24510",]
#we  convert  the data we wan't(emissions) in a convenient  numeric  format
dataBaltimore$Emissions=round(dataBaltimore$Emissions/ 1000,2)
 
#we  get the  data we wan't ( in  particular  there are not  commas  for all  of the  steps
#due  to  space economy
#BALTIMORE 1999
dataBaltimore1999=dataBaltimore[dataBaltimore$year=="1999",]
dataBaltimore1999point=dataBaltimore1999[dataBaltimore1999$type=="POINT",]
               #POINT TOTAL 
dataBaltimore1999pointtotal=sum(dataBaltimore1999point$Emissions)
dataBaltimore1999nonpoint=dataBaltimore1999[dataBaltimore1999$type=="NONPOINT",]
               #NON-POINT TOTAL
dataBaltimore1999nonpointtotal=sum(dataBaltimore1999nonpoint$Emissions)
dataBaltimore1999Nonroad=dataBaltimore1999[dataBaltimore1999$type=="NON-ROAD",]
               #NON-ROAD TOTAL
dataBaltimore1999Nonroadtotal= sum(dataBaltimore1999Nonroad$Emissions)
dataBaltimore1999onroad=dataBaltimore1999[dataBaltimore1999$type=="ON-ROAD",]
              #ON-ROAD TOTAL 
dataBaltimore1999onroadtotal=sum(dataBaltimore1999onroad$Emissions)
#BALTIMORE 2002
dataBaltimore2002=dataBaltimore[dataBaltimore$year=="2002",]
dataBaltimore2002point=dataBaltimore2002[dataBaltimore2002$type=="POINT",]
#POINT TOTAL 
dataBaltimore2002pointtotal=sum(dataBaltimore2002point$Emissions)
dataBaltimore2002nonpoint=dataBaltimore2002[dataBaltimore2002$type=="NONPOINT",]
#NON-POINT TOTAL
dataBaltimore2002nonpointtotal=sum(dataBaltimore2002nonpoint$Emissions)
dataBaltimore2002Nonroad=dataBaltimore2002[dataBaltimore2002$type=="NON-ROAD",]
#NON-ROAD TOTAL
dataBaltimore2002Nonroadtotal= sum(dataBaltimore2002Nonroad$Emissions)
dataBaltimore2002onroad=dataBaltimore2002[dataBaltimore2002$type=="ON-ROAD",]
#ON-ROAD TOTAL 
dataBaltimore2002onroadtotal=sum(dataBaltimore2002onroad$Emissions)
#BALTIMORE 2005
dataBaltimore2005=dataBaltimore[dataBaltimore$year=="2005",]
dataBaltimore2005point=dataBaltimore2005[dataBaltimore2005$type=="POINT",]
#POINT TOTAL 
dataBaltimore2005pointtotal=sum(dataBaltimore2005point$Emissions)
dataBaltimore2005nonpoint=dataBaltimore2005[dataBaltimore2005$type=="NONPOINT",]
#NON-POINT TOTAL
dataBaltimore2005nonpointtotal=sum(dataBaltimore2005nonpoint$Emissions)
dataBaltimore2005Nonroad=dataBaltimore2005[dataBaltimore2005$type=="NON-ROAD",]
#NON-ROAD TOTAL
dataBaltimore2005Nonroadtotal= sum(dataBaltimore2005Nonroad$Emissions)
dataBaltimore2005onroad=dataBaltimore2005[dataBaltimore2005$type=="ON-ROAD",]
#ON-ROAD TOTAL 
dataBaltimore2005onroadtotal=sum(dataBaltimore2005onroad$Emissions)
#BALTIMORE 2008
dataBaltimore2008=dataBaltimore[dataBaltimore$year=="2008",]
dataBaltimore2008point=dataBaltimore2008[dataBaltimore2008$type=="POINT",]
#POINT TOTAL 
dataBaltimore2008pointtotal=sum(dataBaltimore2008point$Emissions)
dataBaltimore2008nonpoint=dataBaltimore2008[dataBaltimore2008$type=="NONPOINT",]
#NON-POINT TOTAL
dataBaltimore2008nonpointtotal=sum(dataBaltimore2008nonpoint$Emissions)
dataBaltimore2008Nonroad=dataBaltimore2008[dataBaltimore2008$type=="NON-ROAD",]
#NON-ROAD TOTAL
dataBaltimore2008Nonroadtotal= sum(dataBaltimore2008Nonroad$Emissions)
dataBaltimore2008onroad=dataBaltimore2008[dataBaltimore2008$type=="ON-ROAD",]
#ON-ROAD TOTAL 
dataBaltimore2008onroadtotal=sum(dataBaltimore2008onroad$Emissions)

library(ggplot2)
data=c(1999,2002,2005,2008)
# we make  4 plots   for  the   requirment and  then we use   the  grid arrange function to   plot them  togheter
totaldataOnRoad=c(dataBaltimore1999onroadtotal,dataBaltimore2002onroadtotal,dataBaltimore2005onroadtotal,dataBaltimore2008onroadtotal)
q1=qplot(data,totaldataOnRoad,color="ON ROAD",geom=c("point", "line"),main="BALTIMORE ON ROAD")
totaldataNOnRoad=c(dataBaltimore1999Nonroadtotal,dataBaltimore2002Nonroadtotal,dataBaltimore2005Nonroadtotal,dataBaltimore2008Nonroadtotal)
q2=qplot(data,totaldataNOnRoad,color="NON ROAD",geom=c("point", "line"),main="BALTIMORE NON ROAD")
totaldataPoint=c(dataBaltimore1999pointtotal,dataBaltimore2002pointtotal,dataBaltimore2005pointtotal,dataBaltimore2008pointtotal)
q3=qplot(data,totaldataPoint,color=" Point",geom=c("point", "line"),main="BALTIMORE POINT")
totaldataNonPoint=c(dataBaltimore1999nonpointtotal,dataBaltimore2002nonpointtotal,dataBaltimore2005nonpointtotal,dataBaltimore2008nonpointtotal)
q4=qplot(data,totaldataNonPoint,color="Non Point",geom=c("point", "line"),main="BALTIMORE NON POINT")
require(gridExtra)
grid.arrange(q1,q2,q3,q4)
plot3=dev.copy(png, 'plot3.png')


 
}











 
