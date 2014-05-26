plot1=function()
{
setwd("C:/Users/Home/Downloads/exdata-data-NEI_data")
nei=readRDS("summarySCC_PM25.rds")
scc=readRDS("Source_Classification_Code.rds")
#we extract  the  dei data  for   the  time periods  required
data1999=nei[nei$year=='1999',]
data2002=nei[nei$year=='2002',]
data2005=nei[nei$year=='2005',]
data2008=nei[nei$year=='2008',]
#we  sum  over  the  total emissions  of this  data
#and   we  convert  it  to an ok format see z
totalem1999=sum(round(data1999$Emissions/1000,2))
totalem2002=sum(round(data2002$Emissions/1000,2))
totalem2005=sum(round(data2005$Emissions/1000,2))
totalem2008=sum(round(data2008$Emissions/1000,2))
#totaem    represents   the concatenation  of all  total emission for   the  perios required
totalem=c(totalem1999,totalem2002,totalem2005,totalem2008)
#in  the   following   lines   we   draw barplots   and on the  top  of  the  barplots  we   draw points 
#shwoing  the total emissions  after    that   we draw   a trend  line
m=barplot(totalem,main="Evollution  of total PM2.5  in US ",ylab="Tons",col=c("red","blue","green","yellow"),ylim=c(0,totalem1999+1000),legend = c("1999", "2002", "2005","2008"),args.legend = list(title = "Legend: Color~Year", x = "topright", cex = .7))
axis(1, at=m,labels=c("1999","2002","2005","2008"))
title(xlab="YEAR")
points(totalem,lw=4,col="black")
text(1,totalem1999+300,totalem1999,cex=0.7)
text(2,totalem2002+300,totalem2002,cex=0.7)
text(3,totalem2005+300,totalem2005,cex=0.7)
text(4,totalem2008+300,totalem2008,cex=0.7)
lines(totalem,lw=1,col="blue",lty=2,cex=0.7)
plot1=dev.copy(png, 'plot1.png')
}
