plot6=function()
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
 
   
  #Los Angeles Nei
  neidata=nei[nei$fips=="06037"&nei$type=="ON-ROAD",]
  indexnei=intersect(neidata$SCC,scc[finalVehicleIndex,]$SCC)
  # #Los Angeles motor  vehice FOR  1999-2005
  la1999motor=sum(round(neidata[neidata$SCC %in% indexnei &neidata$year==1999,]$Emissions,2))
  la2002motor=sum(round(neidata[neidata$SCC %in% indexnei &neidata$year==2002,]$Emissions,2))
  la2005motor=sum(round(neidata[neidata$SCC %in% indexnei &neidata$year==2005,]$Emissions,2))
  la2008motor=sum(round(neidata[neidata$SCC %in% indexnei &neidata$year==2008,]$Emissions,2))
  LaMotor=c(la1999motor,la2002motor,la2005motor,la2008motor)
  #  to compare the  changes  between   the  two  city's  I decided  to make  to   two  3D Pied   in  order to
  #  see  the   differences  from year  to  year  in   %
  install.packages("plotrix")
  library(plotrix)
  slice1=BaltimoreMotor
  slice2=LaMotor
  lb1 = c("1999-","2002-","2005-","2008-")
  lb2 = c("1999-","2002-","2005-","2008-")
  percent1= round(slice1/sum(slice1)*100)
  percent2= round(slice2/sum(slice2)*100)
  lb1=paste(lb1,percent1)
  lb2=paste(lb2,percent2)
  lb1=paste(lb1,"%",sep=" ")
  lb2=paste(lb2,"%",sep=" ")
  par(mfrow=c(1,2))
  pie1=pie3D(slice1,labels = lb1, col=rainbow(length(lb1)),explode=0.08,
             main="Baltimore motor Emissions")
  
  pie2=pie3D(slice2,labels = lb2, col=rainbow(length(lb2)),explode=0.08,
             main="LA motor Emissions")
  plot6=dev.copy(png, 'plot6.png')
  
  
}
