plot4=function()
{
setwd("C:/Users/Home/Downloads/exdata-data-NEI_data")
nei=readRDS("summarySCC_PM25.rds")
scc=readRDS("Source_Classification_Code.rds")
# we use  regular expressions and grep function to   get  the   indexes  from the columns that contain ,, coal ''
indexEiSector=grep("(.*)Coal(*.)|(.*)Coal|^Coal(.*)",scc[,]$EI.Sector)
indexShortName=grep("(.*)Coal(*.)|(.*)Coal|^Coal(.*)",scc[,]$Short.Name)
indexSCC.Levele.Three=grep("(.*)Coal(*.)|(.*)Coal|^Coal(.*)",scc[,]$SCC.Level.Three)
indexSCC.Levele.Four=grep("(.*)Coal(*.)|(.*)Coal|^Coal(.*)",scc[,]$SCC.Level.Four)
# we construct   a  final set of  indices  in the  followin   way
#indexk=set with indexes  of  occurance  of  coal in  col in column
#Where   column  i and   and  columns   k   are the columns  that contains coal in their  description.(ei sector , short name  , second  level  three and  four).
#After   that  I apply the  next logic :
#index1=index1+setdifference(index2+index1) and so   on :D in order  not to   have multiple occurances

 
EiandShort=c(indexEiSector,setdiff(indexShortName,indexEiSector))
EiandShortandLevelThree=c(EiandShort,setdiff(indexSCC.Levele.Three,EiandShort))
EiandShortandLevelThreeAndFour=c(EiandShortandLevelThree,setdiff(indexSCC.Levele.Four,EiandShortandLevelThree))
#I get   the name of the source as indicated by a digit string:
indexSCC=scc[EiandShortandLevelThreeAndFour,]$SCC
nei_coal=nei[nei$SCC==indexSCC,]
#in the next  line   by intersectig the  indexes   we  are sure  that  we  use only indexes
# that  we obtained  in the  last step  from SCC  but   they  have occurances  in  nei$SCC
index_nei=intersect(nei$SCC,indexSCC)
temp=nei[nei$SCC %in% index_nei,]
#finnaly  we extract the data
neiCoal1999=sum(round(temp[temp$year==1999,]$Emissions/1000,2))
neiCoal2002=sum(round(temp[temp$year==2002,]$Emissions/1000,2))
neiCoal2005=sum(round(temp[temp$year==2005,]$Emissions/1000,2))
neiCoal2008=sum(round(temp[temp$year==2008,]$Emissions/1000,2))
data=c(1999,2002,2005,2008)
neiCoal=c(neiCoal1999,neiCoal2002,neiCoal2005,neiCoal2008)
# we   make  a  3 d  pie 
install.packages("plotrix")
library(plotrix)
slices=neiCoal
percent= round(slices/sum(slices)*100)
lb = c("1999-","2002-","2005-","2008-")
lb=paste(lb,percent)
lb=paste(lb,"%",sep=" ")
pie3D(slices,labels = lb, col=rainbow(length(lb)),explode=0.08,
    main="Coal  Combustion U.S.A. 1999-2008")
plot4=dev.copy(png, 'plot4.png')


}





