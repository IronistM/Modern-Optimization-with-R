### bag-tabu.R file ###
library(tabuSearch) # load tabuSearch package
source("functions.R") # load the profit function

# tabu search for bag prices:
D=5 # dimension (number of prices)
MaxPrice=1000
Dim=ceiling(log(MaxPrice,2)) # size of each price (=10)
size=D*Dim # total number of bits (=50)
s=sample(0:1,size,replace=TRUE) # initial search

intbin=function(x) # convert binary to integer
{ sum(2^(which(rev(x==1))-1)) } # explained in Chapter 3
bintbin=function(x) # convert binary to D prices
{ # note: D and Dim need to be set outside this function
  s=vector(length=D) 
  for(i in 1:D) # convert x into s:
  { ini=(i-1)*Dim+1;end=ini+Dim-1
    s[i]=intbin(x[ini:end])
  }
  return(s)
}
bprofit=function(x) # profit for binary x
{ s=bintbin(x) 
  if(sum(s>MaxPrice)>0) f=-Inf # death penalty
  else f=profit(s)
  return(f)
}

cat("initial:",bintbin(s),"f:",bprofit(s),"\n")
s=tabuSearch(size,iters=100,objFunc=bprofit,config=s,neigh=4,listSize=16,nRestarts=1)
b=which.max(s$eUtilityKeep) # best index
cat("best:",bintbin(s$configKeep[b,]),"f:",s$eUtilityKeep[b],"\n")

