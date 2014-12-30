### compare2.R file ###

source("functions.R") # bag prices functions
library(genalg)
library(DEoptim)
library(pso)
library(copulaedas)

# evaluation functions: ------------------------------------
crastrigin=function(x) # adapted rastrigin
{ f=10*length(x)+sum(x^2-10*cos(2*pi*x))
  # global assignment code: <<- 
  EV<<-EV+1 # increase evaluations
  if(f<BEST) BEST<<-f # minimum value
  if(EV<=MAXFN) F[EV]<<-BEST
  return(f)
}
cprofit=function(x) # adapted bag prices
{ x=round(x,digits=0) # convert x into integer
  # given that EDA occasionally produces unbounded values:
  x=ifelse(x<1,1,x)        # assure that x is within 
  x=ifelse(x>1000,1000,x)  # the [1,1000] bounds
  s=sales(x)          # get the expected sales
  c=cost(s)           # get the expected cost
  profit=sum(s*x-c)   # compute the profit
  EV<<-EV+1 # increase evaluations
  if(profit>BEST) BEST<<-profit # maximum value
  if(EV<=MAXFN) F[EV]<<-BEST
  return(-profit) # minimization task!
}
# auxiliary functions: ------------------------------------
crun=function(method,f,lower,upper,LP,maxit) # run a method
{ if(method=="EA") 
     rbga(evalFunc=f,stringMin=lower,stringMax=upper,popSize=LP,
          iters=maxit*1.5)
  else if(method=="DE")  
     { C=DEoptim.control(itermax=maxit,trace=FALSE,NP=LP)
       DEoptim(f,lower=lower,upper=upper,control=C)
     }
  else if(method=="PSO") 
     { C=list(maxit=maxit,s=LP)
       psoptim(rep(NA,length(lower)),fn=f,
               lower=lower,upper=upper,control=C)
     }
  else if(method=="EDA") 
     { setMethod("edaTerminate","EDA",edaTerminateMaxGen)
       GCEDA=CEDA(copula="normal",margin="norm",popSize=LP,
                  maxGen=maxit)
       GCEDA@name="GCEDA"
       edaRun(GCEDA,f,lower,upper)
     }
}

successes=function(x,LIM,type="min") # number of successes
{ if(type=="min") return(sum(x<LIM)) else return(sum(x>LIM)) }

ctest=function(Methods,f,lower,upper,type="min",Runs, # test
               D,MAXFN,maxit,LP,pdf,main,LIM) # all methods:
{ RES=vector("list",length(Methods)) # all results
  VAL=matrix(nrow=Runs,ncol=length(Methods)) # best values
  for(m in 1:length(Methods)) # initialize RES object
   RES[[m]]=matrix(nrow=MAXFN,ncol=Runs) 
 
  for(R in 1:Runs) # cycle all runs
    for(m in 1:length(Methods))
      { EV<<-0; F<<-rep(NA,MAXFN) # reset EV and F
        if(type=="min") BEST<<-Inf else BEST<<- -Inf # reset BEST
        suppressWarnings(crun(Methods[m],f,lower,upper,LP,maxit))
        RES[[m]][,R]=F # store all best values
        VAL[R,m]=F[MAXFN] # store best value at MAXFN
      }
  # compute average F result per method:
  AV=matrix(nrow=MAXFN,ncol=length(Methods))
  for(m in 1:length(Methods))
    for(i in 1:MAXFN)
      AV[i,m]=mean(RES[[m]][i,])
  # show results:
  cat(main,"\n",Methods,"\n")
  cat(round(apply(VAL,2,mean),digits=0)," (average best)\n")
  cat(round(100*apply(VAL,2,successes,LIM,type)/Runs,
            digits=0)," (%successes)\n")

  # create pdf file:
  pdf(paste(pdf,".pdf",sep=""),width=5,height=5,paper="special")
  par(mar=c(4.0,4.0,1.8,0.6)) # reduce default plot margin
  MIN=min(AV);MAX=max(AV)
  # use a grid to improve clarity:
  g1=seq(1,MAXFN,length.out=500) # grid for lines
  plot(g1,AV[g1,1],ylim=c(MIN,MAX),type="l",lwd=2,main=main,
       ylab="average best",xlab="number of evaluations")
  for(i in 2:length(Methods)) lines(g1,AV[g1,i],lwd=2,lty=i)
  if(type=="min") position="topright" else position="bottomright"
  legend(position,legend=Methods,lwd=2,lty=1:length(Methods))
  dev.off() # close the PDF device
}

# define EV, BEST and F:
MAXFN=10000
EV=0;BEST=Inf;F=rep(NA,MAXFN)
# define method labels:
Methods=c("EA","DE","PSO","EDA")
# rastrigin comparison: -----------------------------------
Runs=50; D=20; LP=100; maxit=100
lower=rep(-5.2,D);upper=rep(5.2,D)
ctest(Methods,crastrigin,lower,upper,"min",Runs,D,MAXFN,maxit,LP,
      "comp-rastrigin2","rastrigin (D=20)",75)
# bag prices comparison: ----------------------------------
MAXFN=5000
F=rep(NA,MAXFN)
Runs=50; D=5; LP=50; maxit=100
lower=rep(1,D);upper=rep(1000,D)
ctest(Methods,cprofit,lower,upper,"max",Runs,D,MAXFN,maxit,LP,
      "comp-bagprices","bag prices (D=5)",43500)
