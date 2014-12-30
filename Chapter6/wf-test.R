### wf-test.R file ###

source("mo-tasks.R") # load multi-optimization tasks
library(genalg) # load genalg package

set.seed(12345) # set for replicability

step=5 # number of weight combinations
w=matrix(ncol=2,nrow=step) # weight combinations
w[,1]=seq(1,0,length.out=step)
w[,2]=1-w[,1]

print("Weight combinations:")
print(w)
# --- binary task:
D=8 # 8 bits
eval=function(x) return(W[1]*sumbin(x)+W[2]*maxsin(x))
cat("binary task:\n")
for(i in 1:step)
{
 W= -w[i,] # rbga.bin minimization goal: max. f1 and max. f2
 G=rbga.bin(size=D,popSize=12,iters=100,zeroToOneRatio=1,
            evalFunc=eval,elitism=1)
 b=G$population[which.min(G$evaluations),] # best individual
 cat("w",i,"best:",b)
 cat(" f=(",sumbin(b),",",round(maxsin(b),2),")","\n",sep="")
}

# --- integer task:
D=5 # 5 bag prices 
eval=function(x) return(W[1]*profit(x)+W[2]*produced(x))
cat("integer task:\n")
res=matrix(nrow=nrow(w),ncol=ncol(w)) # for CSV files
for(i in 1:step)
{
 W=c(-w[i,1],w[i,2]) # rbga min. goal: max. f1 and min. f2
 G=rbga(evalFunc=eval,stringMin=rep(1,D),stringMax=rep(1000,D),
        popSize=20,iters=100)
 b=round(G$population[which.min(G$evaluations),]) # best
 cat("w",i,"best:",b)
 cat(" f=(",profit(b),",",produced(b),")","\n",sep="")
 res[i,]=c(profit(b),produced(b))
}
write.table(res,"wf-bag.csv",
            row.names=FALSE,col.names=FALSE,sep=" ")
# --- real value task:
D=8 # dimension
eval=function(x) return(sum(W*fes1(x)))
cat("real value task:\n")
for(i in 1:step)
{
 W=w[i,] # rbga minimization goal
 G=rbga(evalFunc=eval,stringMin=rep(0,D),stringMax=rep(1,D),
        popSize=20,iters=100)
 b=G$population[which.min(G$evaluations),] # best solution
 cat("w",i,"best:",round(b,2))
 cat(" f=(",round(fes1(b)[1],2),",",round(fes1(b)[2],2),")","\n",sep="")
 res[i,]=fes1(b)
}
write.table(res,"wf-fes1.csv",
            row.names=FALSE,col.names=FALSE,sep=" ")
