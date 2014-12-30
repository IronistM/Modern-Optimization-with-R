### bs-sann.R file ###

source("hill.R") # load the hchange method
source("functions.R") # load the profit function
eval=function(x) -profit(x) # optim minimizes!

# hill climbing for all bag prices, one run:
D=5; C=list(maxit=10000,temp=1000,trace=TRUE,REPORT=10000)
s=sample(1:1000,D,replace=TRUE) # initial search
ichange=function(par) # integer value change
{ D=length(par)
  hchange(par,lower=rep(1,D),upper=rep(1000,D),rnorm,mean=0,sd=1)
}
s=optim(s,eval,gr=ichange,method="SANN",control=C)
cat("best:",s$par,"profit:",abs(s$value),"\n")

# hill climbing for sphere, one run:
sphere=function(x) sum(x^2)
D=2; C=list(maxit=10000,temp=1000,trace=TRUE,REPORT=10000)

s=runif(D,-5.2,5.2) # initial search
# SANN with default change (gr) function:
s=optim(s,sphere,method="SANN",control=C)
cat("best:",s$par,"f:",s$value,"\n")
