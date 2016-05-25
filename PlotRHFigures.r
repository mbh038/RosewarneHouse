# Plot Rosewarne House figures

# Hukseflux heat Flux Meter
rhfm=0.00625 # W/m2

# amplifier functions
ampch1<-function(vout){
    0.012 * vout - 0.094 #cal 5-02-16 alice & mike
}
ampch2<-function(vout){
    0.0118 * vout - 0.5235 #cal 5-02-16 alice & mike
}

# read in Unit 1 data
u1<-read.csv("18-03-to-15-04 unit1.prn",sep="\t",stringsAsFactors=FALSE)
id<-seq(1,nrow(u1))
u1<-cbind(id,u1)
names(u1)<-c("id","date","time","T1","T2","T3","T4","hp1","hp2")

library(TTR)

library(rafalib)
library(openintro)
data(COL)
mypar(1,1)

# plot(u1$T2,type="l",col=COL[1]) # outside
# lines(u1$T4,col=COL[2]) # outside
# lines(u1$T1,col=COL[3]) # inside
# lines(u1$T3,col=COL[4]) # inside

tin<-(u1$T1+u1$T3)/2
tin<-SMA(tin,5)
tout<-(u1$T2+u1$T4)/2
tout<-SMA(tout,5)

days<-seq(1,nrow(u1))/1440

png("./figures/temperatures_U1.png",width=595,height=642)
plot(days,tin,
     type="l",
     col=COL[4],
     ylim=c(0,30),
     xlab="Time (days)",
     ylab="Temperature (deg celsius)")
lines(days,tout,col=COL[1])
legend("topright", 
       c("Interior wall surface", "Exterior wall surface"), 
       lwd=2, 
       col=c(COL[4], COL[1])
)
text(0,0,"Unit 1 (D)",pos=4)
dev.off()

# heat plate plots
u1$hp1preamp1<-ampch1(u1$hp1)
u1$hp1flux1<-u1$hp1preamp1/0.06

u1$hp2preamp2<-ampch2(u1$hp2)
u1$hp2flux2<-u1$hp2preamp2/0.06

index=u1$hp1flux1<10
index=index & u1$hp1flux1>-7
index = index & u1$hp2flux2<30
index = index & u1$hp2flux2>0

# 5 min average
u1$hp2flux2<-SMA(u1$hp2flux2,5)
#plot(u1$id[index],u1$hp2flux2[index],col="blue",type="l",ylim=c(0,20))

Qexp<-u1$hp2flux2[index]
Tint<-tin[index]
Text<-tout[index]
t=u1$id[index]

Tm=numeric()
Q=numeric()

mypar(1,1)
R1=1.8
R2=0.804
C=1364000
Tm_init=15.08
tau=60

Qt<-function(R1,R2,Tm_init,C){
    Tm[1]=Tm_init
    for (i in 1:(length(t)-1)){
        dt=60*(t[i+1]-t[i])
        Q[i]=(Tint[i]-Tm[i])/R1
        Tm[i+1]=((Tint[i+1]/R1)+(Text[i+1]/R2)+C*Tm[i]/dt)/(1/R1 + 1/R2 + C/dt)
    }
    Q
}

Q<-Qt(R1,R2,Tm_init,C)
Qexp<-Qexp[-1]

png("./figures/heatflux_U1.png",width=595,height=642)
plot(t[index]/1440,Qexp[index],
     type="l",
     ylim=c(-5,20),
     xlab="Time (days)",
     ylab="Heat flux Q (W/m^2)",
     col=COL[1])
lines(t[index]/1440,Q[index],
      col=COL[2])
legend("topright", 
       c("Measured", "Predicted"), 
       lwd=2, 
       col=c(COL[1], COL[2])
       )
text(0,-5,"Unit 1 (D), R=3.6",pos=4)
dev.off()

## UNIT 4

u4<-read.csv("22-04-to-29-04 unit4 final.prn",sep="\t",stringsAsFactors=FALSE)
id<-seq(1,nrow(u4))
u4<-cbind(id,u4)
names(u4)<-c("id","date","time","T1","T2","T3","T4","hp1","hp2")

tin<-u4$T1
tin<-SMA(tin,5)
tout<-u4$T4
tout<-SMA(tout,5)

library(TTR)
library(rafalib)
library(openintro)
data(COL)
mypar(1,1)

days<-seq(1,nrow(u4))/1440

png("./figures/temperatures_U4.png",width=595,height=642)
plot(days,tin,
     type="l",
     col=COL[4],
     ylim=c(0,30),
     xlab="Time (days)",
     ylab="Temperature (deg celsius)")
lines(days,tout,col=COL[1])
legend("topright", 
       c("Interior wall surface", "Exterior wall surface"), 
       lwd=2, 
       col=c(COL[4], COL[1])
)
text(0,0,"Unit 4 (A)",pos=4)
dev.off()

#index=u4$T2-u4$T4>-1 & u4$T2-u4$T4<0


# heat plate plots
u4$hp1preamp1<-ampch1(u4$hp1)
u4$hp1flux1<-u4$hp1preamp1/0.06

u4$hp2preamp2<-ampch2(u4$hp2)
u4$hp2flux2<-u4$hp2preamp2/0.06

index=2000:10000

# plot(u4$T1[index]-u4$T4[index],type="l")
# lines(u4$T2[index]-u4$T4[index],col="blue")
# 
# plot(u4$hp1flux1,type="l")
# plot(u4$hp2flux2,type="l",col="blue")
# 
# plot(u4$id[index],u4$hp1flux1[index],type="l",ylim=c(-10,10))
# plot(u4$id[index],u4$hp2flux2[index],col="blue",type="l",ylim=c(0,20))

Qexp<-u4$hp2flux2[index]
Tint<-tin[index]
Text<-tout[index]
t=u4$id[index]

Tm=numeric()
Q=numeric()

mypar(1,1)
R1=2.25 # celotex
R2=0.71 # as per unit 1, stone wall
C=1364000
Tm_init=15.08
tau=60

Qt<-function(R1,R2,Tm_init,C){
    Tm[1]=Tm_init
    for (i in 1:length(t)){
        dt=60*(t[i+1]-t[i])
        Q[i]=(Tint[i]-Tm[i])/R1
        Tm[i+1]=((Tint[i+1]/R1)+(Text[i+1]/R2)+C*Tm[i]/dt)/(1/R1 + 1/R2 + C/dt)
    }
    Q
}

Q<-Qt(R1,R2,Tm_init,C)
#Qexp<-Qexp[-1]

Qexp<-Qexp+mean(Q-Qexp)

png("./figures/heatflux_U4.png",width=595,height=642)
plot(t[index]/1440,Qexp[index],
     type="l",
     ylim=c(0,3),
     xlab="Time (days)",
     ylab="Heat flux Q (W/m^2)",
     col=COL[1])
lines(t[index]/1440,Q[index],
      col=COL[2])
legend("topright", 
       c("Measured", "Predicted"), 
       lwd=2, 
       col=c(COL[1], COL[2])
)
text(3,0,"Unit 4 (A), R = 2.96",pos=4)
dev.off()