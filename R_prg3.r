# Loess smooth filter technique for Machine Learning time series analysis
rm(list=ls())
getwd()


prgname = 'loess_smoothers.r'
#set location of the input file
infile <- "/Analysis/R_programs/data_prac1.dat"
#read data
#input <- read.table(infile,header=TRUE,skip=1)
input = read.table(infile,skip=1)
cat(dim(input)); cat("\n")

#length of the input series
npt <- length(input[,1])

# transferring the data into variables
dte <- input[,3]
data <- input[,5]

ll.rough = loess(data~dte, span=0.1)
ll.smooth = loess(data~dte, span=0.75)

pdf('/Analysis/pro_plots/loess_smoothers_r.pdf')
plot(dte,data,col=8,xlab="Year",main="Loess Smoothers",ylim=c(-2.,2.),axes=FALSE)
###Making ticks our own way
ticks<-c(2002,2004,2006,2008,2010,2012,2014,2016)
axis(side=1,at=ticks,labels=ticks)
axis(side=2,at=seq(-2.0,2.0,by=0.5))
box(lty='solid',col='black')

###rectangle with filled color
rect(2002.,-2.1,2003.,2.1,col="gray92",border="transparent")
rect(2009.,-2.1,2010.,2.1,col="gray92",border="transparent")
rect(2014.,-2.1,2015.,2.1,col="gray92",border="transparent")
rect(2005.5,-2.1,2006.5,2.1,col="lightcyan1",border="transparent")
rect(2015.0,-2.1,2016.,2.1,col="lightcyan1",border="transparent")

lines(dte,data,col=8,lwd=2,lty=1)
lines(dte,predict(ll.smooth),col=3,lwd=2,lty=1)
lines(dte,predict(ll.rough),col=4,lwd=2,lty=1)

legend(2002.,2.0,c("Data", "Span = 0.75","Span = 0.10"),
       col = c(8,3,4),lty = c(1,1), lwd = 1.5,cex=0.85)

par(xpd=TRUE)
text(2010.,2.0,prgname,cex=1.0)
par(xpd=FALSE)
dev.off()

