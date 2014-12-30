### gp-rastrigin.R ###

library(rgp) # load rgp

# auxiliary functions:
rastrigin=function(x) 10*length(x)+sum(x^2-10*cos(2*pi*x))
fwrapper=function(x,f) f(x[1],x[2])

# configuration of the genetic programming:
ST=inputVariableSet("x1","x2")
cF1=constantFactorySet(function() rnorm(1)) # mean=0, sd=1
FS=functionSet("+","*","-")
# set the input samples (grid^2 data points):
grid=10 # size of the grid used
domain=matrix(ncol=2,nrow=grid^2) # 2D domain grid
domain[,1]=rep(seq(-5.2,5.2,length.out=grid),each=grid)
domain[,2]=rep(seq(-5.2,5.2,length.out=grid),times=grid)
eval=function(f) # evaluation function
{ mse(apply(domain,1,rastrigin),apply(domain,1,fwrapper,f)) }

# run the genetic programming:
# Note: in the book, the rgp version 0.3-4 version was used;
#       other versions might produce different results
set.seed(12345) # set for replicability
mut=function(func) # set the mutation function
{ mutateSubtree(func,funcset=FS,inset=ST, conset=cF1,
                mutatesubtreeprob=0.1,maxsubtreedepth=4) }
gp=geneticProgramming(functionSet=FS,inputVariables=ST,
                      constantSet=cF1,populationSize=50,
                      fitnessFunction=eval,
                      stopCondition=makeTimeStopCondition(50),
                      mutationFunction=mut,
                      verbose=TRUE)
# show the results:
b=gp$population[[which.min(gp$fitnessValues)]]
cat("best solution (f=",eval(b),"):\n")
print(b)
# create approximation plot:
L1=apply(domain,1,rastrigin);L2=apply(domain,1,fwrapper,b)
MIN=min(L1,L2);MAX=max(L1,L2)
pdf("gp-function.pdf",width=7,height=7,paper="special")
plot(L1,ylim=c(MIN,MAX),type="l",lwd=2,lty=1,
     xlab="points",ylab="function values")
lines(L2,type="l",lwd=2,lty=2)
legend("bottomright",leg=c("rastrigin","GP function"),lwd=2,
       lty=1:2)
dev.off()
