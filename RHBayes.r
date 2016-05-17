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
lines(u1$T4,col=2) # outside
lines(u1$T1,col=3) # inside
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

plot(u1$T1[index]-u1$T2[index],type="l",ylim=c(-3,3))
lines(u1$T3[index]-u1$T4[index],col="blue")

plot(u1$hp1flux1,type="l")
lines(u1$hp2flux2,col="blue")

plot(u1$id[index],u1$hp1flux1[index],type="l",ylim=c(-10,10))
plot(u1$id[index],u1$hp2flux2[index],col="blue",type="l",ylim=c(0,20))

Qexp<-u1$hp2flux2[index]
Tint<-u1$T3[index]
Text<-u1$T4[index]
t=u1$id[index]

Tm=numeric()
Q=numeric()

mypar(1,1)
R1=1.7
R2=1.7
C=1364000
Tm_init=15.5
tau=60

Qt<-function(R1,R2,Tm_init,C){
  Tm[1]=Tm_init
  for (i in 1:(sum(index)-1)){
    dt=60*(t[i+1]-t[i])
    Q[i]=(Tint[i]-Tm[i])/R1
    Tm[i+1]=((Tint[i+1]/R1)+(Text[i+1]/R2)+C*Tm[i]/dt)/(1/R1 + 1/R2 + C/dt)
  }
  Q
}

Q<-Qt(R1,R2,Tm_init,C)
  
plot(Q,type="l",ylim=c(min(min(Q),min(Qexp)),max(max(Q),max(Qexp))))
lines(Qexp,col="blue")

LL <- function(R1,R2,Tm_init,C) {
       R<-Qt(R1,R2,Tm_init,C)
       #
       -sum(log(R),log=TRUE)
    }

library(stats4)
mle(LL, start = list(R1=1.7,R2=1.7,Tm_init=12.5,C=1364000),fixed=list(C),method = "BFGS")


residual=(Q-Qexp[-1])^2
sum(residual)

U<-numeric()
for (i in 1:sum(index)){
    Tintave=cumsum(Tint[i])
    Textave=cumsum(Text[i])
    Qave=cumsum(Qexp[i])
    U[i]=Qave/(Tintave-Textave)
}
plot(U,type="l",ylim=c(0,5))
median(U)

