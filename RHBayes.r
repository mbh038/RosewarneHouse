R1=1
R2=1
C=200000
TMinit=12

u1<-read.csv("18-03-to-15-04 unit1.prn",sep="\t",stringsAsFactors=FALSE)
id<-seq(1,nrow(u1))
u1<-cbind(id,u1)
names(u1)<-c("id","date","time","T1","T2","T3","T4","hp1","hp2")

library(rafalib)
mypar(2,1)
plot(u1$T2,type="l") # outside
lines(u1$T4,col=2) # inside
lines(u1$T1,col=3) # outside
lines(u1$T3,col=4) # inside

plot(u1$T1-u1$T3,type="l",ylim=c(-3,3))
lines(u1$T2-u1$T4,col="blue")
 
#index=u1$T2-u1$T4>-1 & u1$T2-u1$T4<0
 
# amplifier functions
ampch1<-function(vout){
    0.012 * vout - 0.094 #cal 5-02-16 alice & mike
}
ampch2<-function(vout){
    0.0118 * vout - 0.5235 #cal 5-02-16 alice & mike
}

# heat plate plots
u1$hp1preamp1<-ampch1(u1$hp1)
u1$hp1flux1<-u1$hp1preamp1/0.06

u1$hp2preamp2<-ampch2(u1$hp2)
u1$hp2flux2<-u1$hp2preamp2/0.06

index=u1$hp1flux1<10
index=index & u1$hp1flux1>-7
index = index & u1$hp2flux2<30
index = index & u1$hp2flux2>0

plot(u1$T1[index]-u1$T3[index],type="l",ylim=c(-3,3))
lines(u1$T2[index]-u1$T4[index],col="blue")

plot(u1$hp1flux1,type="l")
lines(u1$hp2flux2,col="blue")

plot(u1$id[index],u1$hp1flux1[index],type="l",ylim=c(-10,10))
plot(u1$id[index],u1$hp2flux2[index],col="blue",type="l",ylim=c(0,20))

Qexp<-u1$hp2flux2[index]
Tint<-u1$T4[index]
Text<-u1$T2[index]

Tm=numeric()
Q=numeric()

R1=2
R2=1
C=400000
Tm[1]=10
tau=60

for (i in 1:(sum(index)-1)){
    Q[i]=(Tint[i]-Tm[i])/R1
    Tm[i+1]=((Tint[i+1]/R1)+(Text[i+1]/R2)+C*Tm[i]/tau)/(1/R1 + 1/R2 + C/tau)
}
plot(Q,type="l")
lines(Qexp,col="blue")

U<-numeric()
for (i in 1:sum(index)){
    Tintave=cumsum(Tint[i])
    Textave=cumsum(Text[i])
    Qave=cumsum(Qexp[i])
    U[i]=Qave/(Textave-Tintave)
}
plot(U,type="l")