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
R1=1.847
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
  
plot(Qexp,type="l",ylim=c(min(min(Q),min(Qexp)),max(max(Q),max(Qexp))),xlab="Time (min)",ylab="Heat flux Q (W/m^2)",col="red")
lines(Q,col="blue")
legend("topright", c("Measured", "Predicted"), pch="o", col=c("red", "blue"))


LL <- function(R1,R2,Tm_init,C, mu, sigma) {
    R = Qexp-Qt(R1,R2,Tm_init,C)
    #
    R = suppressWarnings(dnorm(R, mu, sigma, log = TRUE))
    #
    -sum(R)
}


u


Q<-Qt(coef(fit)[1],coef(fit)[2],coef(fit)[3],coef(fit)[4])
lines(Q,col="green")

#Unit mle results
# Call:
#     mle(minuslogl = LL, start = list(R1 = 1.71, R2 = 1, Tm_init = 15.4, 
#                                      C = 1364000, sigma = 1), method = "L-BFGS-B", fixed = list(mu = 0), 
#         nobs = length(Q), lower = c(1, 0.1, 10, 1e+06, 0), upper = c(3, 
#                                                                      3, 20, 2e+06, 5))
# 
# Coefficients:
#     Estimate Std. Error
# R1      1.847540e+00 0.03788928
# R2      8.041011e-01 0.04438734
# Tm_init 1.507905e+01 0.13360146
# C       1.364000e+06        NaN
# sigma   2.754651e+00 0.01044703
# 
# -2 log L: 169103.2

#Unit 4 (A)

R1=1
R2=0.804 # as for unit 1
C=1.364e6 # as for unit 1
TMinit=15.1

u4<-read.csv("22-04-to-29-04 unit4 final.prn",sep="\t",stringsAsFactors=FALSE)
id<-seq(1,nrow(u4))
u4<-cbind(id,u4)
names(u4)<-c("id","date","time","T1","T2","T3","T4","hp1","hp2")

library(rafalib)
mypar(2,1)
plot(u4$T1,type="l") # inside
lines(u4$T2,col=2) # inside
#lines(u4$T3,col=3)
lines(u4$T4,col=4) # outside

plot(u4$T1-u4$T4,type="l")
lines(u4$T2-u4$T4,col="blue")

#index=u4$T2-u4$T4>-1 & u4$T2-u4$T4<0

# amplifier functions
ampch1<-function(vout){
    0.012 * vout - 0.094 #cal 5-02-16 alice & mike
}
ampch2<-function(vout){
    0.0118 * vout - 0.5235 #cal 5-02-16 alice & mike
}

# heat plate plots
u4$hp1preamp1<-ampch1(u4$hp1)
u4$hp1flux1<-u4$hp1preamp1/0.06

u4$hp2preamp2<-ampch2(u4$hp2)
u4$hp2flux2<-u4$hp2preamp2/0.06

index=2000:10000

plot(u4$T1[index]-u4$T4[index],type="l")
lines(u4$T2[index]-u4$T4[index],col="blue")

plot(u4$hp1flux1,type="l")
plot(u4$hp2flux2,type="l",col="blue")

plot(u4$id[index],u4$hp1flux1[index],type="l",ylim=c(-10,10))
plot(u4$id[index],u4$hp2flux2[index],col="blue",type="l",ylim=c(0,20))

Qexp<-u4$hp2flux2[index]
Tint<-u4$T1[index]
Text<-u4$T4[index]
t=u4$id[index]

Tm=numeric()
Q=numeric()

mypar(1,1)
R1=2.9 # celotex
R2=0.804 # as per unit 1, stone wall
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

plot(Qexp,type="l",ylim=c(min(min(Q),min(Qexp)),max(max(Q),max(Qexp))),xlab="Time (min)",ylab="Heat flux Q (W/m^2)",col="red")
lines(Q,col="blue")
legend("topright", c("Measured", "Predicted"), pch="o", col=c("red", "blue"))

LL <- function(R1,R2,Tm_init,C, mu, sigma) {
    R = Qexp-Qt(R1,R2,Tm_init,C)
    #
    R = suppressWarnings(dnorm(R, mu, sigma, log = TRUE))
    #
    -sum(R)
}


library(stats4)
fit<-mle(LL, 
         start = list(R1=2.9,R2=0.804,Tm_init=15.4,C=1364000,sigma=1),
         fixed=list(mu=0),
         nobs = length(Q),
         lower = c(2, .5,14,1000000,0.1), 
         upper = c(4,1.5,17,2000000,5),
         method= "L-BFGS-B"
         )


Q<-Qt(coef(fit)[1],coef(fit)[2],coef(fit)[3],coef(fit)[4])
lines(Q,col="green")

#Unit 4 mle results
# Maximum likelihood estimation
# 
# Call:
#     mle(minuslogl = LL, start = list(R1 = 2.9, R2 = 0.804, Tm_init = 15.4, 
#                                      C = 1364000, sigma = 1), method = "L-BFGS-B", fixed = list(mu = 0), 
#         nobs = length(Q), lower = c(2, 0.5, 14, 1e+06, 0.1), upper = c(4, 
#                                                                        1.5, 17, 2e+06, 5))
# 
# Coefficients:
#     Estimate   Std. Error
# R1      2.720612e+00          NaN
# R2      7.231661e-01 0.0051402457
# Tm_init 1.498265e+01          NaN
# C       1.364000e+06 4.6717021928
# sigma   1.000000e-01 0.0002112173
# 
# -2 log L: 9159.363 