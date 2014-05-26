plot5=function()
{
  
setwd("C:/Users/Home/Downloads/exdata-data-NEI_data")
  nei=readRDS("summarySCC_PM25.rds" )
  scc=readRDS("Source_Classification_Code.rds")
  # get    the    values   that contain   vehicle  in  EI.Sector and Short.Name
indexVehiclefromEiSector=grep("(.*)[Vv]ehicle(*.)|(.*)[Vv]ehicle|^[Vv]ehicle(.*)",scc[,]$EI.Sector)
indexVehiclefromShortName=grep("(.*)[Vv]ehicle(*.)|(.*)[Vv]ehicle|^[Vv]ehicle(.*)",scc[,]$Short.Name)
  #  we  union  this   sets of indices  in order to obtain  non-repetative final subset
  #  with occurence  of  the  vehicle in  the description
finalVehicleIndex=union(indexVehiclefromEiSector,indexVehiclefromShortName)
#Baltimore NEI
neidata=nei[nei$fips=="24510"&nei$type=="ON-ROAD",]
indexnei=intersect(neidata$SCC,scc[finalVehicleIndex,]$SCC)
#Baltimore  motor  vehice FOR  1999-2005
baltimore1999motor=sum(round(neidata[neidata$SCC %in% indexnei &neidata$year==1999,]$Emissions,2))
baltimore2002motor=sum(round(neidata[neidata$SCC %in% indexnei &neidata$year==2002,]$Emissions,2))
baltimore2005motor=sum(round(neidata[neidata$SCC %in% indexnei &neidata$year==2005,]$Emissions,2))
baltimore2008motor=sum(round(neidata[neidata$SCC %in% indexnei &neidata$year==2008,]$Emissions,2))
#making  the plot
library(ggplot2)
data=c(1999,2002,2005,2008)
BaltimoreMotor=c(baltimore1999motor,baltimore2002motor,baltimore2005motor,baltimore2008motor)
dataplot=data.frame(x=data,y=BaltimoreMotor)
baltiplot=ggplot(dataplot,aes(x=data,y=BaltimoreMotor),legend="Baltimore  Motor ")+geom_line()+xlab("Period")+ylab("Emissions")+ggtitle("Baltimore Vehicles")
baltiplot
plot5=dev.copy(png, 'plot5.png')


}
