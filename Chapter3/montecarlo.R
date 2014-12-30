### montecarlo.R file ###

# montecarlo uniform search method
#    N - number of samples
#    lower - vector with lowest values for each dimension
#    upper - vector with highest values for each dimension
#    domain - vector list of size D with domain values
#    FUN - evaluation function
#    type - "min" or "max"
#    ... - extra parameters for FUN
mcsearch=function(N,lower,upper,FUN,type="min",...)
{ D=length(lower)
  s=matrix(nrow=N,ncol=D) # set the search space 
  for(i in 1:N) s[i,]=runif(D,lower,upper)
  fsearch(s,FUN,type,...) # best solution
}
