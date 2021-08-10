# Quantile Regression analysis 
#This is an example for reading different type of outputs in R
library(quantreg)
library('R.matlab')
library(ggplot2)
prgname= "quantile_spi_hwmi_hist.r"
###output of Precipitation and drought.m
#setwd('/Analysis/Heat_waves/results/')
tas<-readMat('/home/jayanthikishore/Desktop/Heatwaves/results_May2021/spi_hwmid_hist_720x360_concurnt.mat')
mylist=as.data.frame(tas)
rm(tas)
attach(mylist)

###Precipitation and Heatwave (temperature) relation plot (Scatter plot)
pdf('/home/jayanthikishore/Desktop/Heatwaves/results_May2021/spi_hwmid_hist_concurnt_1951-2005_fig3b.pdf')
# pdf('/Analysis/test/ps2eps/Heat_waves/Figure_3b.pdf')
# cex.main: Size of main title
# cex.lab: Size of axis labels (the text describing the axis)
# cex.axis: Size of axis text (the values that indicate the axis tick labels)
plot(spimax1,heatmax1,xlab=" ",ylab="Heatwave",type = "n", xlim = c(-3.0,4.0),
	ylim=c(0.,20.),cex.lab=1.4,cex.axis=1.45,font.axis=2,pch=50,cex=.5, main = "Hist HWMId and IMD SPI")
points(spimax1,heatmax1,cex=.5,col="blue")

# taus <- c(.1,.3,0.7,.9,0.99,0.999)
#taus <- c(.1,.3,0.7,.9,0.99)
taus <- c(.1,.3,0.7,.9)
nv <- length(taus)
xx <- seq(min(spimax1),max(spimax1),0.1)
f <- coef(rq((heatmax1)~(spimax1),tau=taus))

colors <- rainbow(5)
palette(colors)
yy <- cbind(1,xx)%*%f
for(i in 1:length(taus)){
	lines(xx,yy[,i],col=colors[i], lwd=1.75)
}
legend(3.1, 7.5, taus, 1:6, col=colors, title = "Tau",cex=0.95,text.font=2)

# Locate positions for the text insertion 
amat <- matrix(taus,nrow=1)
#text(-2.6,0.37, amat[1,1])
#text(-2.6,1.01, amat[1,2])
#text(-2.6,2.45, amat[1,3])
#text(-2.6,3.15, amat[1,4])
#text(-2.7,5.35, amat[1,5])
# text(-1.80,5.4, amat[1,6])

abline(lm(heatmax1 ~ spimax1),col="black", lwd=1.5, lty = 2)
abline(rq(heatmax1 ~ spimax1), col="black", lwd=1.5)
legend(1.45,10.8,c("mean (LSE) fit", "median (LAE) fit"),
       col = c("black","black"),lty = c(2,1), lwd = 1.5,cex=1.0,text.font=2.0)

par(xpd=TRUE)
text(3.975,19.95,"(b)",cex=1.2,text.font=2)
text(2.85,21.3,prgname,cex=0.65)
# text(-2.5,-1.5,"o/p:hwmi_spi_con_heat_drought_720x360.m",cex=0.75)
par(xpd=FALSE)
dev.off()
###Quantile regression plot
pdf('/home/jayanthikishore/Desktop/Heatwaves/results_May2021/hist_quantile_concurnt_1951-2005_fig3d.pdf')
# pdf('/Analysis/test/ps2eps/Heat_waves/Figure_3d.pdf')
par(mgp=c(2,1,0))		#this one axes labels closer to the plot
plot(summary(rq(heatmax1~spimax1,tau = 1:49/50,data=mylist)),text.font=2,xlab="Quantile",
	ylab="Slope",cex=0.9,font.axis=2, main = "SPI03",cex.lab=2.5,cex.axis=3.45,font.lab=2,ylim=c(-5.0,4.0))
par(mgp=c(3,1,0))		#reset the default
par(xpd=TRUE)
text(0.95,-0.6,"(d)",cex=1.2,pos=4,text.font=2)
# text(0.1,-1.1,prgname,cex=0.6)
# text(0.15,-0.95,"o/p:hwmi_spi_con_heat_drought_1mnthprec_720x360.m",cex=0.65)
par(xpd=FALSE)
dev.off()