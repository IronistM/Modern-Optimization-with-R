### binary-tabu.R file ###
library(tabuSearch) # load tabuSearch package

# tabu search for sum of bits:
sumbin=function(x) (sum(x)) # sum of bits
D=8 # dimension
s=rep(0,D) # c(0,0,0,0,...)
s=tabuSearch(D,iters=2,objFunc=sumbin,config=s,neigh=2,
             listSize=4,nRestarts=1)
b=which.max(s$eUtilityKeep) # best index
cat("best:",s$configKeep[b,],"f:",s$eUtilityKeep[b],"\n")

# tabu search for max sin:
intbin=function(x) sum(2^(which(rev(x==1))-1))
maxsin=function(x) # max sin (explained in Chapter 3)
{ D=length(x);x=intbin(x); return(sin(pi*(as.numeric(x))/(2^D))) }
D=8
s=rep(0,D) # c(0,0,0,0,...)
s=tabuSearch(D,iters=2,objFunc=maxsin,config=s,neigh=2,
             listSize=4,nRestarts=1)
b=which.max(s$eUtilityKeep) # best index
cat("best:",s$configKeep[b,],"f:",s$eUtilityKeep[b],"\n")
