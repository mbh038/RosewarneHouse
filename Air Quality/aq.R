
ua<-read.csv("unitA.csv",stringsAsFactors=FALSE,header=FALSE)
ub<-read.csv("uB.csv",stringsAsFactors=FALSE,header=FALSE)
uc<-read.csv("uC.csv",stringsAsFactors=FALSE,header=FALSE)
ud<-read.csv("uD.csv",stringsAsFactors=FALSE,header=FALSE)

#align time series
ua$t=(seq(length.out=7210)+586)/60/24
ub$t=(seq(length.out=4256)+615)/60/24
uc$t=(seq(length.out=10017)+563)/60/24
ud$t=(seq(length.out=9805)+821)/60/24

library(rafalib)
library(openintro)
data(COL)
plot(ua$t,ua$V2,
     type="l",
     ylim=c(300,500),
     col=COL[1],
     xlab="Time (days)",
     # ylab="CO2 concentration (ppm)"
     ylab=expression(paste("CO"[2]," concentration (ppm)"))
)
lines(ub$t,ub$V2,col=COL[2])
lines(uc$t,uc$V2,col=COL[4])
lines(ud$t,ud$V2,col=COL[6])
legend("bottomleft", 
       c("A", "B","C","D"), 
       lwd=2, 
       col=c(COL[1], COL[2],COL[4],COL[6]),
       cex=1.0
)
