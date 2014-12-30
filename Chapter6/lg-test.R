### lg-test.R file ###

source("mo-tasks.R") # load multi-optimization tasks
source("lg-ga.R") # load lrgba.bin
set.seed(12345) # set for replicability

LEXI=c(0.2,0.2) # tolerance 20% for each goal
cat("tolerance thresholds:",LEXI,"\n")

# --- binary task:
D=8 # 8 bits
# eval: transform binary objectives into minimization goal
#       returns a vector with 2 values, one per objective:
eval=function(x) return(c(-sumbin(x),-maxsin(x)))
popSize=12
G=lrbga.bin(size=D,popSize=popSize,iters=100,zeroToOneRatio=1,
            evalFunc=eval,elitism=1)
print("Ranking of last population:")
B=tournament(G$population,eval,k=popSize,n=popSize,m=2)
for(i in 1:popSize)
{
 x=G$population[B[i],]
 cat(x," f=(",sumbin(x),",",round(maxsin(x),2),")","\n",sep="")
}
