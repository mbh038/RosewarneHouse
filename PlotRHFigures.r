# Plot Rosewarne House figures

# Hukseflux heat Flux Meter

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
mypar(b=2,a=2)
library(openintro)
data(COL)
#mypar(1,1)

# plot(u1$T2,type="l",col=COL[1]) # outside
# lines(u1$T4,col=COL[2]) # outside
# lines(u1$T1,col=COL[3]) # inside
# lines(u1$T3,col=COL[4]) # inside

tin<-(u1$T1+u1$T3)/2
tin<-SMA(tin,5)
tout<-(u1$T2+u1$T4)/2
tout<-SMA(tout,5)

# heat plate plots
u1$hp1preamp1<-ampch1(u1$hp1)
u1$hp1flux1<-u1$hp1preamp1/0.06

u1$hp2preamp2<-ampch2(u1$hp2)
u1$hp2flux2<-u1$hp2preamp2/0.06

index=u1$hp1flux1<10
index=index & u1$hp1flux1>-7
index = index & u1$hp2flux2<30
index = index & u1$hp2flux2>0

days<-seq(1,nrow(u1))/1440

#mypar(2,2)
#par(mfcol=c(2,2)) 

plot(days[index],tin[index],
     type="l",
     col=COL[4],
     ylim=c(0,30),
     xlab="Time (days)",
     ylab=expression(paste("Temperature "^"o","C"))
)
lines(days[index],tout[index],col=COL[1])
legend("bottomright", 
       c("Interior wall surface", "Exterior wall surface"), 
       lwd=2, 
       col=c(COL[4], COL[1]),
       cex=0.7
       )
text(0,0,"Unit 1 (D)",pos=4,cex=0.9)
#dev.off()

# 5 min average
u1$hp2flux2<-SMA(u1$hp2flux2,20)
#plot(u1$id[index],u1$hp2flux2[index],col="blue",type="l",ylim=c(0,20))

Qexp<-u1$hp2flux2[index]
Tint<-tin[index]
Text<-tout[index]
t=u1$id[index]

Tm=numeric()
Q=numeric()

#mypar(1,1)
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

#png("./figures/heatflux_U1.png",width=595,height=642)
plot(t[index]/1440,Qexp[index],
     type="l",
     ylim=c(-5,20),
     xlab="Time (days)",
     ylab=expression(paste('Heat Flux (W/',m^2,')',sep='')),
     col=COL[6])
lines(t[index]/1440,Q[index],
      col=COL[2])
legend("bottomright", 
       c("Measured", "Predicted"), 
       lwd=2, 
       col=c(COL[6], COL[2]),
       cex=0.7
       )
text(0,-5,"Unit 1 (D), R=2.65",pos=4,cex=0.9)
#dev.off()

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
#mypar(1,1)

days<-seq(1,nrow(u4))/1440

index=2000:10000
#png("./figures/temperatures_U4.png",width=595,height=642)
plot(days[index],tin[index],
     type="l",
     col=COL[4],
     ylim=c(0,30),
     xlim=c(3,7),
     xlab="Time (days)",
     ylab=expression(paste("Temperature "^"o","C"))
     )
lines(days[index],tout[index],col=COL[1])
legend("bottomright", 
       c("Interior wall surface", "Exterior wall surface"), 
       lwd=2, 
       col=c(COL[4], COL[1]),
       cex=0.7
       )
text(3,0,"Unit 4 (A)",pos=4,cex=0.9)
#dev.off()

#index=u4$T2-u4$T4>-1 & u4$T2-u4$T4<0


# heat plate plots
u4$hp1preamp1<-ampch1(u4$hp1)
u4$hp1flux1<-u4$hp1preamp1/0.06

u4$hp2preamp2<-ampch2(u4$hp2)
u4$hp2flux2<-u4$hp2preamp2/0.06

# 5 min average
u4$hp2flux2<-SMA(u4$hp2flux2,20)

index=4320:10000

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

#mypar(1,1)
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

#png("./figures/heatflux_U4.png",width=595,height=642)
plot(t/1440,Qexp,
     type="l",
     ylim=c(0,3),
     xlim=c(3,7),
     xlab="Time (days)",
     ylab=expression(paste('Heat Flux (W/',m^2,')',sep='')),
     col=COL[6])
lines(t/1440,Q,
      col=COL[2])
legend("bottomright", 
       c("Measured", "Predicted"), 
       lwd=2, 
       col=c(COL[6], COL[2]),
       cex=0.7
       )
text(3,0,"Unit 4 (A), R = 2.96",pos=4,cex=0.9)

png("./figures/paper/U1U4v2.png",width=595,height=550)
dev.off()

## UNIT 4 pre-retrofit

u4p<-read.table("unit4preRetrofit30-11.csv",sep=",",stringsAsFactors=FALSE,header=TRUE)
id<-seq(1,nrow(u4p))
u4p<-cbind(id,u4p)
names(u4p)<-c("id","datetime","T1","T2","T3","T4","hp1","hp2")

index=200:(nrow(u4p)-20)
library(TTR)
tin<-u4p$T2
tin<-SMA(tin,5)
tout<-u4p$T4
tout<-SMA(tout,5)


library(rafalib)
library(openintro)
data(COL)
mypar(1,1)

days<-seq(1,nrow(u4p))/1440

png("./figures/temperatures_U4preRetrofit.png",width=595,height=642)
plot(days[index],tin[index],
     type="l",
     col=COL[4],
     ylim=c(5,15),
     xlab="Time (days)",
     ylab=expression(paste("Temperature "^"o","C"))
     )
lines(days[index],tout[index],col=COL[1])
legend("topright", 
       c("Interior wall surface", "Exterior wall surface"), 
       lwd=2, 
       col=c(COL[4], COL[1])
)
text(0,5,"Unit 4 (A) (no insulation)",pos=4)
dev.off()

#index=u4$T2-u4$T4>-1 & u4$T2-u4$T4<0


# heat plate plots
u4p$hp1preamp1<-ampch1(u4p$hp1)
u4p$hp1flux1<-u4p$hp1preamp1/0.06

u4p$hp2preamp2<-ampch2(u4p$hp2)
u4p$hp2flux2<-u4p$hp2preamp2/0.06

index=200:(nrow(u4p)-20)

Qexp<--u4p$hp2flux2[index]+15
Tint<-tin[index]
Text<-tout[index]
t=u4p$id[index]

Tm=numeric()
Q=numeric()

mypar(1,1)
R1=.55 # celotex
R2=0.22 # as per unit 1, stone wall
C=1364
Tm_init=5.65
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

png("./figures/heatflux_U4preRetrofit.png",width=595,height=642)
plot(t[index]/1440,Qexp[index],
     type="l",
     xlim=c(0,3.5),
     ylim=c(0,8),
     xlab="Time (days)",
     #ylab=parse(text=paste0("Heat Flux W/m","^2")),
     ylab=expression(paste('Heat Flux (W/',m^2,')',sep='')),
     col=COL[1])
lines(t[index]/1440,Q[index],
      col=COL[2])
legend("topright", 
       c("Measured", "Predicted"), 
       lwd=2, 
       col=c(COL[1], COL[2])
)
text(0,0,"Unit 4 (A) pre-insulation, R = 0.76",pos=4)
dev.off()

sse<-c(2829,10132,1971)
ssc<-c(576,886,627.2)
de<-c(2873,3114,1972)
dc<-c(585,622,627)

### Usage figures

emb<-c(458,458,138,138,183,183,80,80,792,792,298,298)
#A ,D ,NEW BUILD
# iu<-c(48.3,47.4,8.9,8.8,87.1,58.6,16.1,10.8,22.1,22.1,4.1,4.1)
iu<-c(48.3,47.4,8.9,8.8,76.6,44.3,14.2,8.19,22.1,22.1,4.1,4.1)

sb<-data.frame(cbind(emb,iu))

sb$Cottage<-as.factor(c(rep("A",4),rep("D",4),rep("N",4)))
sb$model<-as.factor(rep(c("Steady State Heat Flow Model","Dynamic Heat Flow Model"),6))
sb$model = factor(sb$model,levels(sb$model)[c(2,1)]) # reorder the factors
eExp<-expression(paste('Lifetime Energy (kWh/',m^2,')',sep=''))
cExp<-expression(paste('Lifetime Carbon (kg C',O[2],'/',m^2,')',sep=''))
sb$metric=as.factor(rep(c("Specific Lifetime Energy (kWh)","Specific Lifetime Energy (kWh)","Specific Lifetime Carbon (kgCO2)","Specific Lifetime Carbon (kgCO2)"),3))
sb$metric = factor(sb$metric,levels(sb$metric)[c(2,1)]) # reorder the factors

sb<-sb[c(3,5,4,1,2)] # reorder the columns
lifetime<-50
sb$lifetime<-sb$emb/lifetime+sb$iu

library(ggplot2)

png("./figures/paper/EC.png",width=595,height=350)
g<-ggplot(data=sb,aes(x=Cottage,y=lifetime,fill=Cottage))+geom_bar(stat="identity")+facet_grid(metric~ model,scales="free")

g<-g+theme(axis.title.y=element_blank(),legend.position="none")
g
dev.off()

#Air Quality


