### sphere-EDA.R file ###
library(copulaedas)

sphere=function(x) sum(x^2)

D=2; maxit=10; LP=5
set.seed(12345) # set for replicability 

# set termination criterion and report method:
setMethod("edaTerminate","EDA",edaTerminateMaxGen)
setMethod("edaReport","EDA",edaReportSimple)

# set EDA type:
UMDA=CEDA(copula="indep",margin="norm",popSize=LP,maxGen=maxit)
UMDA@name="UMDA (LP=5)"
# run the algorithm:
E=edaRun(UMDA,sphere,rep(-5.2,D),rep(5.2,D))
# show result:
show(E)
cat("best:",E@bestSol,"f:",E@bestEval,"\n")

# second EDA execution, using LP=100:
maxit=10; LP=100;
UMDA=CEDA(copula="indep",margin="norm",popSize=LP,maxGen=maxit)
UMDA@name="UMDA (LP=100)"
setMethod("edaReport","EDA",edaReportDumpPop) # pop_*.txt files
E=edaRun(UMDA,sphere,rep(-5.2,D),rep(5.2,D))
show(E)
cat("best:",E@bestSol,"f:",E@bestEval,"\n")

# read dumped files and create a plot:
pdf("eda1.pdf",width=7,height=7)
j=1; # j-th parameter
i=1;d=read.table(paste("pop_",i,".txt",sep=""))
plot(xlim=c(1,maxit),rep(1,LP),d[,j],pch=19,
     xlab="iterations",ylab=paste("s_",j," value",sep=""))
for(i in 2:maxit) 
{ d=read.table(paste("pop_",i,".txt",sep=""))
  points(rep(i,LP),d[,j],pch=19)
}
dev.off()
