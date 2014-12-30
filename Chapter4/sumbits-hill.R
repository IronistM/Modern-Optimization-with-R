### sumbits-hill.R file ###

source("hill.R") # load the hill climbing methods

# sum a raw binary object x (evaluation function):
sumbin=function(x) sum(x)

# hill climbing for sum of bits, one run:
D=8 # dimension
s=rep(0,D) # c(0,0,0,0,...)
C=list(maxit=10,REPORT=1) # maximum of 10 iterations
ichange=function(par,lower,upper) # integer change
{ hchange(par,lower,upper,rnorm,mean=0,sd=1) }

hclimbing(s,sumbin,change=ichange,lower=rep(0,D),upper=rep(1,D),
          control=C,type="max")
