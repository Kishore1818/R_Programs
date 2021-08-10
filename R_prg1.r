#This program for Empirical decomposition techniques for Machine Learning project

# Load Library
library(EMD)

# Set cdir to the directory with the lombscargle program
cdir <- "/Analysis/R_programs/"
# set the working directory to this directory
setwd(cdir)
source("LombScargle.R")

# Set infile to be the input dataset
# here is CSR Precipitation dataset corrected with SM09 GIA
infile <- "/Analysis/R_programs/GIS_Simp_l1sean_nogld_oce_scaled_114.txt" 
# read data
input <- read.table(infile)

# length of the input time series
nmon <- length(input[,1])
# time array for dataset
tdec <- input[,1]
# mass values
mass <- input[,2]

# This will show the sifting procedure for the dataset
# quartz()
# tryimf <- extractimf(mass, tdec, boundary="wave",check=TRUE)

# This will plot the time-series, the IMFs and the residual
quartz()

mass.emd<-emd(mass,tdec,boundary="wave")
rangeimf <- range(mass.emd$imf)
par(mfrow=c(mass.emd$nimf+2, 1), mar=c(3,2,2,1))
plot(tdec,mass,type="l")
title(main = list("Precipitation Change: CSR RL05, GIA: SM09", font = 4))
for(i in 1:mass.emd$nimf) {
  plot(tdec,mass.emd$imf[,i],type="l",ylim=rangeimf,ylab="Mass [Gt]")
  title(main = list(paste("IMF Results ",as.character(i)), font = 4))
}
  plot(tdec,mass.emd$residue,type="l",ylab="Mass [Gt]",xlab="Year")
title(main = list("Precipitation Residual", font = 4))
# dev.off()

# This will plot the Hilbert-spectra
# xaxis: time
# yaxis: instantaneous period of the signal
# color: amplitude of the signal at that point
quartz()
hs1<-hilbertspec(mass.emd$imf,tdec)
par(mfrow=c(mass.emd$nimf, 1), mar=c(3,2,2,1))
for(i in 1:mass.emd$nimf) {
  spectrogram(hs1$amplitude[,i], 1/hs1$instantfreq[,i], tdec)
  plotTitle <- paste("Hilbert Spectrum and Decomposition function", as.character(i)," Location: CSR RL05, GIA: SM09",sep="")
  title(main = list(plotTitle, font = 4))  
}
# dev.off()

# Lomb-scargle analysis of IMFs
# Define global value used by Lomb-Scargle to label plots
# this unit is the unit of time for converting to frequency
unit <<- "year"

# Frequencies usually easier to define in terms of 1/Period.
# In this case since "dominant" frequency is expected to correspond
# to 24-hour period, let's search frequencies corresponding to
# periods from 0.25 year to 12 years.
MinFrequency <- 1/12
MaxFrequency <- 4

# See if MaxFrequency is perhaps too high
if (MaxFrequency > 1/(2*mean(diff(tdec))))
{
  cat("MaxFrequency may be above Nyquist limit.\n")
}

# 3. Process the IMF time series
for(i in 1:mass.emd$nimf) {
  Expression <- t(mass.emd$imf[,i])
  plotTitle <- paste("Time series IMF ", as.character(i),": CSR RL05, GIA: SM09",sep="")
  # Check for consistency between Expression and Time
  N <- ncol(Expression)  # number of data points
  stopifnot(length(tdec) == N) # stopping if length time not equal to length data
  # 2.4  Define number of test frequencies:  normally 2N or 4N
  M <- 4*N
  
  # Frequencies to test in Lomb-Scargle analysis
  TestFrequencies <- MinFrequency +
    (MaxFrequency - MinFrequency) * (0:(M-1) / (M-1))
  
  # Estimate of number of independent frequencies in Lomb-Scargle
  # analysis based on sample size.  From Horne and Baliunas,
  # "A Prescription for Period Analysis of Unevenly Sampled Time Series",
  # The Astrophysical Journal, 392: 757-763, 1986.
  Nindependent <- Nindependent <- NHorneBaliunas(sum(!is.na(Expression)))
  
  quartz()
#   pdf(file=paste("CSR_GRN_EMD_IMF_",as.character(i),"_LOMB-SCARGLE.pdf",sep=""))
  ComputeAndPlotLombScargle(tdec, Expression, TestFrequencies, Nindependent, plotTitle, yLabel="Mass [Gt]")
#   dev.off()
}

