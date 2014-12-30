### tsf.R file ###
library(RCurl) # load RCurl package

# get sunspot series
txt=getURL("http://sidc.oma.be/silso/DATA/yearssn.dat")
# consider 1700-2012 years (remove 2013 * row that is provisory in 2014)
series=strsplit(txt,"\n")[[1]][1:(2012-1700+1)]
cat(series,sep="\n",file="sunspots.dat") # save to file
series=read.table("sunspots.dat")[,2] # read from file

L=length(series) # series length
forecasts=32 # number of 1-ahead forecasts 
outsamples=series[(L-forecasts+1):L] # out-of-samples
sunspots=series[1:(L-forecasts)] # in-samples

# mean absolute error of residuals
maeres=function(residuals) mean(abs(residuals))

# fit best ARIMA model:
INIT=10 # initialization period (no error computed before)
library(forecast) # load forecast package
arima=auto.arima(sunspots) # detected order is AR=2, MA=1
print(arima) # show ARIMA model
cat("arima fit MAE=",
    maeres(arima$residuals[INIT:length(sunspots)]),"\n")
# one-step ahead forecasts:
# (this code is needed because forecast function
#  only issues h-ahead forecasts)
LIN=length(sunspots) # length of in-samples
f1=rep(NA,forecasts)
for(h in 1:forecasts)
  { # execute arima with fixed coefficients but with more in-samples:
   arima1=arima(series[1:(LIN+h-1)],order=arima$arma[c(1,3,2)],fixed=arima$coef)
   f1[h]=forecast(arima1,h=1)$mean[1]
  }
e1=maeres(outsamples-f11)
text1=paste("arima (MAE=",round(e1,digits=1),")",sep="")

# fit genetic programming arithmetic model:
library(rgp) # load rgp
ST=inputVariableSet("x1","x2") # same order of AR arima component
cF1=constantFactorySet(function() rnorm(1)) # mean=0, sd=1
FS=functionSet("+","*","-","/") # arithmetic

# genetic programming time series function
#   receives function f 
#   if(h>0) then returns 1-ahead forecasts
#   else returns MAE over fitting period (in-samples)
gpts=function(f,h=0)
{ 
  if(h>0) TS=series
  else TS=series[1:LIN]
  LTS=length(TS)
  F=rep(0,LTS) # forecasts
  E=rep(0,LTS) # residuals
  if(h>0) I=(LTS-h+1):LTS # h forecasts
  else I=INIT:LTS # fit to in-samples 
  for(i in I)
    { 
     F[i]=f(TS[i-1],TS[i-2])
     if(is.nan(F[i])) F[i]=0 # deal with NaN
     E[i]=TS[i]-F[i]
    }
  if(h>0) return (F[I]) # forecasts
  else return(maeres(E[I])) # MAE on fit
}

# mutation function
mut=function(func)
{ mutateSubtree(func,funcset=FS,inset=ST,conset=cF1,
                mutatesubtreeprob=0.3,maxsubtreedepth=4)}

set.seed(12345) # set for replicability
gp=geneticProgramming(functionSet=FS,inputVariables=ST,
                      constantSet=cF1,
                      populationSize=100,
                      fitnessFunction=gpts,
                      stopCondition=makeStepsStopCondition(1000),
                      mutationFunction=mut,
                      verbose=TRUE)
f2=gpts(gp$population[[which.min(gp$fitnessValues)]],h=forecasts)
e2=maeres(outsamples-f2)

text2=paste("gp (MAE=",round(e2,digits=1),")",sep="")
cat("best solution:\n")
print(gp$population[[which.min(gp$fitnessValues)]])
cat("gp fit MAE=",min(gp$fitnessValues),"\n")

# show quality of one-step ahead forecasts: 
ymin=min(c(outsamples,f1,f2))
ymax=max(c(outsamples,f1,f2))
pdf("fsunspots.pdf")
par(mar=c(4.0,4.0,0.1,0.1))
plot(outsamples,ylim=c(ymin,ymax),type="b",pch=1,
     xlab="time (years after 1980)",ylab="values",cex=0.8)
lines(f1,lty=2,type="b",pch=3,cex=0.5)
lines(f2,lty=3,type="b",pch=5,cex=0.5)
legend("topright",c("sunspots",text1,text2),lty=1:3,pch=c(1,3,5))
dev.off()
