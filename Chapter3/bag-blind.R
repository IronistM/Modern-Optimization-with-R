### bag-blind.R file ###

source("blind.R") # load the blind search methods
source("functions.R") # load profit(), cost() and sales()

# auxiliary function that sets the optimum price for
# one bag type (D), assuming an independent influence of
# a particular price on the remaining bag prices:
ibag=function(D) # D - type of bag
{ x=1:1000 # price for each bag type
  # set search space for one bag:
  search=matrix(ncol=5,nrow=1000)
  search[]=1; search[,D]=x 
  S1=fsearch(search,profit,"max") 
  S1$sol[D] # best price
}

# compute the best price for all bag types:
S=sapply(1:5,ibag) 
# show the optimum solution:
cat("optimum s:",S,"f:",profit(S),"\n")
