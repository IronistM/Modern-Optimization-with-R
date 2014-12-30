### compare.R file ###

source("hill.R") # get hchange
source("blind.R") # get fsearch
source("montecarlo.R") # get mcsearch
library(rminer) # get meanint

# comparison setup:
crastrigin=function(x)
{ f=10*length(x)+sum(x^2-10*cos(2*pi*x))
  # global assignment code: <<- 
  EV<<-EV+1 # increase evaluations
  if(f<BEST) BEST<<-f
  if(EV<=MAXIT) F[EV]<<-BEST
  return(f)
}
Runs=50; D=20; MAXIT=10000
lower=rep(-5.2,D);upper=rep(5.2,D)
rchange1=function(par,lower,upper) # change for hclimbing
{ hchange(par,lower=lower,upper=upper,rnorm,
          mean=0,sd=0.5,round=FALSE) }
rchange2=function(par)             # change for optim
{ hchange(par,lower=lower,upper=upper,rnorm,
          mean=0,sd=0.5,round=FALSE) }
CHILL=list(maxit=MAXIT,REPORT=0)
CSANN=list(maxit=MAXIT,temp=10,trace=FALSE)
Methods=c("monte carlo","hill climbing","simulated annealing")

# run all optimizations and store results:
RES=vector("list",length(Methods)) # all results
for(m in 1:length(Methods))
   RES[[m]]=matrix(nrow=MAXIT,ncol=Runs) 
for(R in 1:Runs) # cycle all runs
{ s=runif(D,-5.2,5.2) # initial search point
  EV=0; BEST=Inf; F=rep(NA,MAXIT) # reset these vars.
  # monte carlo:
  mcsearch(MAXIT,lower=lower,upper=upper,FUN=crastrigin)
  RES[[1]][,R]=F
  # hill climbing:
  EV=0; BEST=Inf; F=rep(NA,MAXIT)
  hclimbing(s,crastrigin,change=rchange1,lower=lower,
            upper=upper,control=CHILL,type="min")
  RES[[2]][,R]=F
  # SANN:
  EV=0; BEST=Inf; F=rep(NA,MAXIT)
  optim(s,crastrigin,method="SANN",gr=rchange2,control=CSANN)
  RES[[3]][,R]=F
}

# aggregate (average and confidence interval) results:
AV=matrix(nrow=MAXIT,ncol=length(Methods))
CI=AV
for(m in 1:length(Methods))
{
 for(i in 1:MAXIT)
 {
  mi=meanint(RES[[m]][i,])
  AV[i,m]=mi$mean;CI[i,m]=mi$int
 }
}

# show comparative PDF graph:

# plot a nice confidence interval bar:
confbar=function(x,ylower,yupper,K=100)
{ segments(x-K,yupper,x+K)
  segments(x-K,ylower,x+K)
  segments(g2,ylower,g2,yupper)
}

pdf("comp-rastrigin.pdf",width=5,height=5)
par(mar=c(4.0,4.0,0.1,0.6)) # reduce default plot margin
MIN=min(AV-CI);MAX=max(AV+CI)
# 10.000 are too much points, thus two grids are used
# to improve clarity:
g1=seq(1,MAXIT,length.out=1000) # grid for lines
g2=seq(1,MAXIT,length.out=11) # grid for confbar
plot(g1,AV[g1,3],ylim=c(MIN,MAX),type="l",lwd=2,
     ylab="average best",xlab="number of evaluations")
confbar(g2,AV[g2,3]-CI[g2,3],AV[g2,3]+CI[g2,3])
lines(g1,AV[g1,2],lwd=2,lty=2)
confbar(g2,AV[g2,2]-CI[g2,2],AV[g2,2]+CI[g2,2])
lines(g1,AV[g1,1],lwd=2,lty=3)
confbar(g2,AV[g2,1]-CI[g2,1],AV[g2,1]+CI[g2,1])
legend("topright",legend=rev(Methods),lwd=2,lty=1:3)
dev.off() # close the PDF device
