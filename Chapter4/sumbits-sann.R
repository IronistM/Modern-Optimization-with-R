### sumbits-sann.R file ###
source("hill.R") # get hchange function
# sum a raw binary object x (evaluation function):
minsumbin=function(x) (length(x)-sum(x)) # optim only minimizes!

# SANN for sum of bits, one run:
D=8 # dimension
s=rep(0,D) # c(0,0,0,0,...)
C=list(maxit=10,temp=10,tmax=1,trace=TRUE,REPORT=1)
bchange=function(par) # binary change
{ D=length(par)
  hchange(par,lower=rep(0,D),upper=rep(1,D),rnorm,mean=0,sd=1)
}
s=optim(s,minsumbin,gr=bchange,method="SANN",control=C)
cat("best:",s$par,"f:",s$value,"(max: fs:",sum(s$par),")\n")
