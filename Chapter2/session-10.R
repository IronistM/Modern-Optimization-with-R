source("functions.R")  # load the code
x=1:5      # show the factorial of 1:5
cat(sapply(x,fact),"\n")
m=matrix(ncol=5,nrow=2)
m[1,]=c(1,1,1,1,1)   # very cheap bags
m[2,]=c(414,404,408,413,395) # optimum
# show profit for both price setups:
y=apply(m,1,profit); print(y) 
