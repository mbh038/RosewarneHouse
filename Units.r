### Analyses of Stable Block units at Rosewarne House

### Unit 1 pre-insulation

u1p<-read.csv("30-11.prn",sep="\t",stringsAsFactors=FALSE)
id<-seq(1,nrow(u1p))
u1p<-cbind(id,u1p)
names(u1p)<-c("id","date","time","T1","T2","T3","T4","hp1","hp2")

library(rafalib)
mypar(2,1)
plot(u1p$T1,type="l") # outside
lines(u1p$T2,col=2) # inside
lines(u1p$T3,col=3) # inside
lines(u1p$T4,col=4) # outside


### Unit 1

u1<-read.csv("18-03-to-15-04 unit1.prn",sep="\t",stringsAsFactors=FALSE)
id<-seq(1,nrow(u1))
u1<-cbind(id,u1)
names(u1)<-c("id","date","time","T1","T2","T3","T4","hp1","hp2")

library(rafalib)
mypar(2,1)
plot(u1$T1,type="l") # inside
lines(u1$T2,col=2) # outside
lines(u1$T3,col=3) # inside
lines(u1$T4,col=4) # outside

u1$Tin=(u1$T1+u1$T3)/2

u1$dT1=u1$T1-u1$T2
u1$dT2=u1$T3-u1$T4

plot(u1$id,u1$dT1,type="l")
lines(u1$id,u1$dT2,col="red")
plot(u1$hp1,type="l")
lines(u1$hp2,col="red")

index1=u1$dT1>12
index2=u1$dT2>12

plot(u1$id[index1],u1$hp1[index1],type="l")
lines(u1$id[index2],u1$hp2[index2],col="red")

ampch1<-function(vout){
  0.012 * vout - 0.094 #cal 5-02-16 alice & mike
}
ampch2<-function(vout){
  0.0118 * vout - 0.5235 #cal 5-02-16 alice & mike
}

u1$hp1preamp1<-ampch1(u1$hp1)
u1$hp1flux1<-u1$hp1preamp1/0.06

u1$hp2preamp2<-ampch2(u1$hp2)
u1$hp2flux2<-u1$hp2preamp2/0.06


library(rafalib)
mypar(2,1)
plot(u1$T1[index1],u1$hp1flux1[index1],ylim=c(-10,20))
fit1<-lm(u1$hp1flux1[index1]~u1$T1[index1])
abline(fit1$coefficients[1],fit1$coefficients[2],col="red")
fit1$coefficients[2]

plot(u1$T3[index2],u1$hp2flux2[index2],ylim=c(-10,20))
fit2<-lm(u1$hp2flux2[index2]~u1$T3[index2])
abline(fit2$coefficients[1],fit2$coefficients[2],col="red")
fit2$coefficients[2]


library(rafalib)
mypar(2,1)
plot(u1$dT1[index1],u1$hp1flux1[index1],ylim=c(-20,20))
fit3<-lm(u1$hp1flux1[index1]~u1$dT1[index1])
abline(fit3$coefficients[1],fit3$coefficients[2],col="red")
fit3$coefficients[2]


plot(u1$dT2[index2],u1$hp2flux2[index2],ylim=c(-20,20))
fit4<-lm(u1$hp2flux2[index2]~u1$dT2[index2])
abline(fit4$coefficients[1],fit4$coefficients[2],col="red")
fit4$coefficients[2]

### Unit 2

u2<-read.csv("unit 2_5-02 p2.prn",sep="\t",stringsAsFactors=FALSE)
id<-seq(1,nrow(u2))
u2<-cbind(id,u2)
names(u2)<-c("id","date","time","T1","T2","T3","T4","hp1","hp2")

library(rafalib)
mypar(2,1)
plot(u2$T1,type="l") # inside
lines(u2$T2,col=2) # outside
lines(u2$T3,col=3) # inside
lines(u2$T4,col=4) # outside

u2$Tin=(u2$T1+u2$T3)/2

u2$dT1=u2$T1-u2$T2
u2$dT2=u2$T3-u2$T4

mypar(2,1)
plot(u2$id,u2$dT1,type="l")
lines(u2$id,u2$dT2,col="red")
plot(u2$hp1,type="l")
lines(u2$hp2,col="red")

index1=u2$hp1<100 & u2$id>10000 & u2$id<20000
index2=u2$hp2<100 & u2$id>10000 & u2$id<20000

plot(u2$id[index1],u2$hp1[index1],type="l")
lines(u2$id[index2],u2$hp2[index2],col="red")

plot(u2$hp1,type="l")
lines(u2$hp2,col="red")

ampch1<-function(vout){
  0.012 * vout + 0.094 #cal 5-02-16 alice & mike
}
ampch2<-function(vout){
  0.0118 * vout - 0.5235 #cal 5-02-16 alice & mike
}

u2$hp1preamp1<-ampch1(u2$hp1)
u2$hp1flux1<-u2$hp1preamp1/0.06

u2$hp2preamp2<-ampch2(u2$hp2)
u2$hp2flux2<-u2$hp2preamp2/0.06

plot(u2$id[index1],u2$hp1flux1[index1],type="l")
lines(u2$id[index2],u2$hp2flux2[index2],col="red")

#index=10000:20000

mypar(2,1)
plot(u2$T1[index1],u2$hp1flux1[index1],ylim=c(-50,50))
fit1<-lm(u2$hp1flux1[index1]~u2$T1[index1])
abline(fit1$coefficients[1],fit1$coefficients[2],col="red")
fit1$coefficients[2]

plot(u2$T3[index2],u2$hp2flux2[index2],ylim=c(-50,50))
fit2<-lm(u2$hp2flux2[index2]~u2$T3[index2])
abline(fit2$coefficients[1],fit2$coefficients[2],col="red")
fit2$coefficients[2]

library(rafalib)
mypar(2,1)
plot(u2$dT1[index1],u2$hp1flux1[index1],ylim=c(-20,20))
fit3<-lm(u2$hp1flux1[index1]~u2$dT1[index1])
abline(fit3$coefficients[1],fit3$coefficients[2],col="red")
fit3$coefficients[2]

plot(u2$dT2[index2],u2$hp2flux2[index2],ylim=c(-20,20))
fit4<-lm(u2$hp2flux2[index2]~u2$dT2[index2])
abline(fit4$coefficients[1],fit4$coefficients[2],col="red")
fit4$coefficients[2]

### Unit 3

u3<-read.csv("26-02-to-18-03 unit3 2-3test.prn",sep="\t",stringsAsFactors=FALSE)
id<-seq(1,nrow(u3))
u3<-cbind(id,u3)
names(u3)<-c("id","date","time","T1","T2","T3","T4","hp1","hp2")

library(rafalib)
mypar(2,1)
plot(u3$T1,type="l") # inside
lines(u3$T2,col=2) # outside
lines(u3$T3,col=3) # inside
lines(u3$T4,col=4) # outside

u3$Tin=(u3$T1+u3$T3)/2

u3$dT1=u3$T1-u3$T2
u3$dT2=u3$T3-u3$T4

mypar(2,1)
plot(u3$id,u3$dT1,type="l")
lines(u3$id,u3$dT2,col="red")
plot(u3$hp1,type="l")
lines(u3$hp2,col="red")

index1=u3$dT1>11 & u3$id>10000 & u3$id<30000
index2=u3$dT2>11 & u3$id>10000 & u3$id<30000

plot(u3$id[index1],u3$hp1[index1],type="l")
lines(u3$id[index2],u3$hp2[index2],col="red")

plot(u3$hp1,type="l")
lines(u3$hp2,col="red")

ampch1<-function(vout){
  0.012 * vout - 0.094 #cal 5-02-16 alice & mike
}
ampch2<-function(vout){
  0.0118 * vout - 0.5235 #cal 5-02-16 alice & mike
}

u3$hp1preamp1<-ampch1(u3$hp1)
u3$hp1flux1<-u3$hp1preamp1/0.06

u3$hp2preamp2<-ampch2(u3$hp2)
u3$hp2flux2<-u3$hp2preamp2/0.06

#index=10000:20000

mypar(2,1)
plot(u3$T1[index1],u3$hp1flux1[index1],ylim=c(-10,10))
fit1<-lm(u3$hp1flux1[index1]~u3$T1[index1])
abline(fit1$coefficients[1],fit1$coefficients[2],col="red")
fit1$coefficients[2]

plot(u3$T3[index2],u3$hp2flux2[index2],ylim=c(-10,10))
fit2<-lm(u3$hp2flux2[index2]~u3$T3[index2])
abline(fit2$coefficients[1],fit2$coefficients[2],col="red")
fit2$coefficients[2]

library(rafalib)
mypar(2,1)
plot(u3$dT1[index1],u3$hp1flux1[index1],ylim=c(-20,20))
fit3<-lm(u3$hp1flux1[index1]~u3$dT1[index1])
abline(fit3$coefficients[1],fit3$coefficients[2],col="red")
fit3$coefficients[2]

plot(u3$dT2[index2],u3$hp2flux2[index2],ylim=c(-20,20))
fit4<-lm(u3$hp2flux2[index2]~u3$dT2[index2])
abline(fit4$coefficients[1],fit4$coefficients[2],col="red")
fit4$coefficients[2]

### Unit 4

u4<-read.csv("22-04-to-29-04 unit4 final.prn",sep="\t",stringsAsFactors=FALSE)
names(u4)<-c("date","time","T1","T2","T3","T4","hp1","hp2")

library(rafalib)
mypar(2,1)
plot(u4$T1,type="l")
lines(u4$T2,col=2)
#lines(u4$T3,col=3)
lines(u4$T4,col=4)

u4$Tin=(u4$T1+u4$T2)/2

u4$dT=u4$Tin-u4$T4

index=2000:10000

ampch1<-function(vout){
0.012 * vout - 0.094 #cal 5-02-16 alice & mike
}
ampch2<-function(vout){
  0.0118 * vout - 0.5235 #cal 5-02-16 alice & mike
}

u4$hp1preamp1<-ampch1(u4$hp2)
u4$hp1flux1<-u4$hp1preamp1/0.06

u4$hp2preamp2<-ampch2(u4$hp2)
u4$hp2flux2<-u4$hp2preamp2/0.06


plot(u4$Tin[index],u4$hp1flux1[index])
fit1<-lm(u4$hp1flux1[index]~u4$Tin[index])
abline(fit1$coefficients[1],fit1$coefficients[2],col="red")
fit1$coefficients[2]

plot(u4$Tin[index],u4$hp2flux2[index])
fit2<-lm(u4$hp2flux2[index]~u4$Tin[index])
abline(fit2$coefficients[1],fit2$coefficients[2],col="red")
fit2$coefficients[2]
