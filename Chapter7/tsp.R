### tsp.R file ###

library(TSP) # load TSP package
library(RCurl) # load RCurl package
source("oea.R") # load ordered evolutionary algorithm

# get Qatar - 194 cities TSP instance:
txt=getURL("http://www.math.uwaterloo.ca/tsp/world/qa194.tsp")
# simple parse of txt object, removing header and last line:
txt=strsplit(txt,"NODE_COORD_SECTION") # split text into 2 parts
txt=txt[[1]][2] # get second text part
txt=strsplit(txt,"EOF") # split text into 2 parts
txt=txt[[1]][1] # get first text part
# save data into a simple .csv file, sep=" ":
cat(txt,file="qa194.csv")
# read the TSP format into Data 
# (first row is empty, thus header=TRUE)
# get city Cartesian coordinates

Data=read.table("qa194.csv",sep=" ")
Data=Data[,3:2] # longitude and latitude
names(Data)=c("x","y") # x and y labels
N=nrow(Data) # number of cities

# distance between two cities (EUC_2D-norm)
# Eulidean distance rounded to whole number
D=dist(Data,upper=TRUE)
D[1:length(D)]=round(D[1:length(D)])
# create TSP object from D:
TD=TSP(D)

set.seed(12345) # for replicability
cat("2-opt run:\n")
PTM=proc.time() # start clock
R1=solve_TSP(TD,method="2-opt") 
sec=(proc.time()-PTM)[3] # get seconds elapsed
print(R1) # show optimum
cat("time elapsed:",sec,"\n")

MAXIT=100000
Methods=c("SANN","EA","LEA") # comparison of 3 methods
RES=matrix(nrow=MAXIT,ncol=length(Methods)) 
MD=as.matrix(D)

# overall distance of a tour (evaluation function):
tour=function(s)
{ # compute tour length:
  EV<<-EV+1 # increase evaluations
  s=c(s,s[1]) # start city is also end city
  res=0
  for(i in 2:length(s)) res=res+MD[s[i],s[i-1]]
  # store memory with best values:
  if(res<BEST) BEST<<-res
  if(EV<=MAXIT) F[EV]<<-BEST
  # only for hybrid method:
  # return tour
  return(res)
}

# move city index according to dir
mindex=function(i,dir,s=NULL,N=length(s)) 
{ res=i+dir #positive or negative jump
  if(res<1) res=N+res else if(res>N) res=res-N
  return(res)
}

# local improvement and evaluation: 
#   first tries to improve a solution with a 
#   local search that uses domain knowledge (MD)
#   returns best solution and evaluation value
local_imp_tour=function(s,p=NA)
{ # local search 
 N=length(s); ALL=1:N
 if(is.na(p)) p=sample(ALL,1) # select random position 
 I=setdiff(ALL,p)
 
 # current distance: p to neighbors
 pprev=mindex(p,-1,N=N); pnext=mindex(p,1,N=N)
 dpcur=MD[s[pprev],s[p]]+MD[s[p],s[pnext]]
 # new distance if p is remove to another position:
 dpnew=MD[s[pprev],s[pnext]]

 # search for best insertion position for p:
 ibest=0;best=-Inf
 for(i in I) # extra cycle that increases computation
 {
  inext=mindex(i,1,N=N);iprev=mindex(i,-1,N=N)
  if(inext==p) inext=pnext
  if(iprev==p) iprev=pprev
  # dinew: new distance p to neighbors if p inserted:
  # current i distance without p:
  if(i<p) { dinew=MD[s[iprev],s[p]]+MD[s[p],s[i]]
            dicur=MD[s[iprev],s[i]]
          }
  else 
    { dinew=MD[s[i],s[p]]+MD[s[p],s[inext]]
      dicur=MD[s[i],s[inext]]
    }
  # difference between current tour and new one:
  dif=(dicur+dpcur)-(dinew+dpnew)

  if(dif>0 && dif>best) # improved solution
    {
      best=dif
      ibest=i
    }
 } 

 if(ibest>0) # insert p in i
   s=insertion(s,p=p,i=ibest)
 return(list(eval=tour(s),solution=s))
}

# SANN:
cat("SANN run:\n")
set.seed(12345) # for replicability
s=sample(1:N,N) # initial solution
EV=0; BEST=Inf; F=rep(NA,MAXIT) # reset these vars.
C=list(maxit=MAXIT,temp=2000,trace=TRUE,REPORT=MAXIT)
PTM=proc.time() # start clock
SANN=optim(s,fn=tour,gr=insertion,method="SANN",control=C)
sec=(proc.time()-PTM)[3] # get seconds elapsed
cat("time elapsed:",sec,"\n")
RES[,1]=F

# EA:
cat("EA run:\n")
set.seed(12345) # for replicability
EV=0; BEST=Inf; F=rep(NA,MAXIT) # reset these vars.
pSize=30;iters=ceiling((MAXIT-pSize)/(pSize-1))
PTM=proc.time() # start clock
OEA=oea(size=N,popSize=pSize,iters=iters,evalFunc=tour,crossfunc=ox,mutfunc=insertion,REPORT=iters,elitism=1)
sec=(proc.time()-PTM)[3] # get seconds elapsed
cat("time elapsed:",sec,"\n")
RES[,2]=F

# Lamarckian EA (LEA):
cat("LEA run:\n")
set.seed(12345) # for replicability
EV=0; BEST=Inf; F=rep(NA,MAXIT) # reset these vars.
pSize=30;iters=ceiling((MAXIT-pSize)/(pSize-1))
PTM=proc.time() # start clock
LEA=oea(size=N,popSize=pSize,iters=iters,evalFunc=local_imp_tour,crossfunc=ox,mutfunc=insertion,REPORT=iters,elitism=1)
sec=(proc.time()-PTM)[3] # get seconds elapsed
cat("time elapsed:",sec,"\n")
RES[,3]=F

# create PDF with comparison:
pdf("qa194-opt.pdf",paper="special")
par(mar=c(4.0,4.0,0.1,0.1))
X=seq(1,MAXIT,length.out=200)
ylim=c(min(RES)-50,max(RES))
plot(X,RES[X,1],ylim=ylim,type="l",lty=3,lwd=2,xlab="evaluations",ylab="tour distance")
lines(X,RES[X,2],type="l",lty=2,lwd=2)
lines(X,RES[X,3],type="l",lty=1,lwd=2)
legend("topright",Methods,lwd=2,lty=3:1)
dev.off()

# create 3 PDF files with best tours:
pdf("qa194-2-opt.pdf",paper="special")
par(mar=c(0.0,0.0,0.0,0.0))
plot(Data[c(R1[1:N],R1[1]),],type="l",xaxt="n",yaxt="n")
dev.off()
pdf("qa194-ea.pdf",paper="special")
par(mar=c(0.0,0.0,0.0,0.0))
b=OEA$population[which.min(OEA$evaluations),]
plot(Data[c(b,b[1]),],type="l",xaxt="n",yaxt="n")
dev.off()
pdf("qa194-lea.pdf",paper="special")
par(mar=c(0.0,0.0,0.0,0.0))
b=LEA$population[which.min(LEA$evaluations),]
plot(Data[c(b,b[1]),],type="l",xaxt="n",yaxt="n")
dev.off()
