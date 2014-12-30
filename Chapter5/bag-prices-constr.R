### bag-prices-constr.R file ###

source("functions.R") # bag prices functions
library(copulaedas) # EDA

# evaluation function:  ------------------------------------
cprofit2=function(x) # bag prices with death penalty
{ x=round(x,digits=0) # convert x into integer
  x=ifelse(x<1,1,x)        # assure that x is within 
  x=ifelse(x>1000,1000,x)  # the [1,1000] bounds
  s=sales(x)
  if(sum(s)>50) res=Inf # if needed, death penalty!!!
  else{ c=cost(s);profit=sum(s*x-c)
        # if needed, store best value
        if(profit>BEST) { BEST<<-profit; B<<-x}
        res=-profit # minimization task!
      }
  EV<<-EV+1 # increase evaluations
  if(EV<=MAXFN) F[EV]<<-BEST
  return(res)
}
# example of a local search method that repairs a solution:
localRepair=function(eda, gen, pop, popEval, f, lower, upper)
{
 for(i in 1:nrow(pop))
 { x=pop[i,]
   x=round(x,digits=0) # convert x into integer
   x=ifelse(x<lower[1],lower[1],x) # assure x within 
   x=ifelse(x>upper[1],upper[1],x) # bounds
   s=sales(x)
   if(sum(s)>50)
   {
    x1=x
    while(sum(s)>50) # new constraint: repair
    { # increase price to reduce sales:
      x1=x1+abs(round(rnorm(D,mean=0,sd=5)))
      x1=ifelse(x1>upper[1],upper[1],x1) # bound if needed
      s=sales(x1)
    } 
    x=x1 # update the new x
   }
   pop[i,]=x;popEval[i]=f(x)
 } 
 return(list(pop=pop,popEval=popEval))
}

# experiment: ----------------------------------------------
MAXFN=5000 
Runs=50; D=5; LP=50; maxit=100
lower=rep(1,D);upper=rep(1000,D)
Methods=c("Death","Repair")
setMethod("edaTerminate","EDA",edaTerminateMaxGen)
GCEDA=CEDA(copula="normal",margin="norm",popSize=LP,
           maxGen=maxit,fEvalStdDev=10)
GCEDA@name="GCEDA"

RES=vector("list",length(Methods)) # all results
VAL=matrix(nrow=Runs,ncol=length(Methods)) # best values
for(m in 1:length(Methods)) # initialize RES object
   RES[[m]]=matrix(nrow=MAXFN,ncol=Runs) 
for(R in 1:Runs) # cycle all runs
  { 
    B=NA;EV=0; F=rep(NA,MAXFN); BEST= -Inf # reset vars. 
    setMethod("edaOptimize","EDA",edaOptimizeDisabled)
    setMethod("edaTerminate","EDA",edaTerminateMaxGen)
    suppressWarnings(edaRun(GCEDA,cprofit2,lower,upper))
    RES[[1]][,R]=F # store all best values
    VAL[R,1]=F[MAXFN] # store best value at MAXFN

    B=NA;EV=0; F=rep(NA,MAXFN); BEST= -Inf # reset vars. 
    # set local repair search method:
    setMethod("edaOptimize","EDA",localRepair)
    # set additional termination criterion:
    setMethod("edaTerminate","EDA",
              edaTerminateCombined(edaTerminateMaxGen,edaTerminateEvalStdDev))
    # this edaRun might produces warnings or errors:
    suppressWarnings(try(edaRun(GCEDA,cprofit2,lower,upper),silent=TRUE))
    if(EV<MAXFN) # if stopped due to EvalStdDev
       F[(EV+1):MAXFN]=rep(F[EV],MAXFN-EV) # replace NAs
    RES[[2]][,R]=F # store all best values
    VAL[R,2]=F[MAXFN] # store best value at MAXFN
  }

# compute average F result per method:
AV=matrix(nrow=MAXFN,ncol=length(Methods))
for(m in 1:length(Methods))
  for(i in 1:MAXFN)
    AV[i,m]=mean(RES[[m]][i,])
# show results:
cat(Methods,"\n")
cat(round(apply(VAL,2,mean),digits=0)," (average best)\n")
# Mann-Whitney non-parametric test:
p=wilcox.test(VAL[,1],VAL[,2],paired=TRUE)$p.value
cat("p-value:",round(p,digits=2),"(<0.05)\n")

# create pdf file:
pdf("comp-bagprices-constr.pdf",width=5,height=5,paper="special")
par(mar=c(4.0,4.0,1.8,0.6)) # reduce default plot margin
# use a grid to improve clarity:
g1=seq(1,MAXFN,length.out=500) # grid for lines
plot(g1,AV[g1,2],type="l",lwd=2,
     main="bag prices with constraint",
     ylab="average best",xlab="number of evaluations")
lines(g1,AV[g1,1],lwd=2,lty=2)
legend("bottomright",legend=rev(Methods),lwd=2,lty=1:4)
dev.off() # close the PDF device
