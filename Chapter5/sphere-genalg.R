### sphere-genalg.R file ###
library(genalg) # load genalg

# evolutionary algorithm for sphere:
sphere=function(x) sum(x^2)
D=2
monitor=function(obj)
{ if(i==1) 
    { plot(obj$population,xlim=c(-5.2,5.2),ylim=c(-5.2,5.2),
           xlab="x1",ylab="x2",type="p",pch=16,
           col=gray(1-i/maxit)) 
    }
  else if(i%%K==0) points(obj$population,pch=16,
                          col=gray(1-i/maxit))
  i<<-i+1 # global update
}

maxit=100
K=5 # store population values every K generations
i=1 # initial generation

# evolutionary algorithm execution:
pdf("genalg2.pdf",width=5,height=5)
set.seed(12345) # set for replicability purposes 
E=rbga(rep(-5.2,D),rep(5.2,D),popSize=5,iters=maxit,
       monitorFunc=monitor,evalFunc=sphere)
b=which.min(E$evaluations) # best individual
cat("best:",E$population[b,],"f:",E$evaluations[b],"\n")
dev.off()
