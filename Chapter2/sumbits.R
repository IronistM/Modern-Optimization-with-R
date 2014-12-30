# create PDF file:
DIR=""             # change if different directory is used
pdf(paste(DIR,"sumbits.pdf",sep=""),width=5,height=5)

sumbinint=function(x)          # sum of bits of an integer
{ return(sum(as.numeric(intToBits(x))))}

sumbits=function(x)              # sum of bits of a vector
{ return(sapply(x,sumbinint))}

D=8; x=0:(2^D-1) # x is the search space (integer representation)
y=sumbits(x)     # y is the number of binary bits of x
plot(x,y,type="l",ylab="evaluation function",
     xlab="search space (x)",lwd=2)
pmax=c(x[which.max(y)],max(y)) # maximum point coordinates
points(pmax[1],pmax[2],pch=19,lwd=2)  # plot maximum point
legend("topleft","optimum",pch=19,lwd=2)    # add a legend
dev.off()                               # close the device
