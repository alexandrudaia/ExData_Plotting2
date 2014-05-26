plot2=function(){
setwd("C:/Users/Home/Downloads/exdata-data-NEI_data")
nei=readRDS("summarySCC_PM25.rds")
scc=readRDS("Source_Classification_Code.rds")
dataBaltimore=nei[nei$fips=="24510",]
dataBaltimore$Emissions=round(dataBaltimore$Emissions/ 1000,2)
dataBaltimore1999=dataBaltimore[dataBaltimore$year=="1999",]
dataBaltimore2002=dataBaltimore[dataBaltimore$year=="2002",]
dataBaltimore2005=dataBaltimore[dataBaltimore$year=="2005",]
dataBaltimore2008=dataBaltimore[dataBaltimore$year=="2008",]

totalBaltimore1999=sum(dataBaltimore1999$Emissions)
totalBaltimore2002=sum(dataBaltimore2002$Emissions)
totalBaltimore2005=sum(dataBaltimore2005$Emissions)
totalBaltimore2008=sum(dataBaltimore2008$Emissions)
totalBaltimore=c(totalBaltimore1999,totalBaltimore2002,totalBaltimore2005,totalBaltimore2008)
m=barplot(totalBaltimore,main="Evollution  of total PM2.5 in Baltimore ",ylim=c(0,5.0),ylab="Tons",col=c("cyan4","darkgray","cyan2","cornsilk2"),legend = c("1999", "2002", "2005","2008"),args.legend = list(title = "Legend: Color~Year", x = "topright", cex = .7))
axis(1, at=m,labels=c("1999","2002","2005","2008"))
title(xlab="YEAR")
points(totalBaltimore,lw=4,col="black")
text(1,totalBaltimore1999+0.25,totalBaltimore1999,cex=0.7)
text(2,totalBaltimore2002+0.25,totalBaltimore2002,cex=0.7)
text(3,totalBaltimore2005+0.25,totalBaltimore2005,cex=0.7)
text(4,totalBaltimore2008+0.25,totalBaltimore2008,cex=0.7)
lines(totalBaltimore,lw=1,col="blue",lty=2,cex=0.7)
plot2=dev.copy(png, 'plot2.png')


}
