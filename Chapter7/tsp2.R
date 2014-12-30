### tsp2.R file ###
# this file assumes that tsp.R has already been executed

library(rgeos) # get gArea function

poly=function(data)
{ poly="";sep=", "
  for(i in 1:nrow(data))
  { if(i==nrow(data)) sep=""
    poly=paste(poly,paste(data[i,],collapse=" "),sep,sep="")
  }
  poly=paste("POLYGON((",poly,"))",collapse="")
  poly=readWKT(poly) # WKT format to polygon
}

# new evaluation function: area of polygon
area=function(s) return( gArea(poly(Data[c(s,s[1]),])) )

cat("area of 2-opt TSP tour:",area(R1),"\n")

# plot area of 2-opt:
pdf("qa-2opt-area.pdf",paper="special")
par(mar=c(0.0,0.0,0.0,0.0))
PR1=poly(Data[c(R1,R1[1]),])
plot(PR1,col="gray")
dev.off()

# EA:
cat("EA run for TSP area:\n")
set.seed(12345) # for replicability
pSize=30;iters=20
PTM=proc.time() # start clock
OEA=oea(size=N,popSize=pSize,iters=iters,evalFunc=area,crossfunc=ox,mutfunc=insertion,REPORT=iters,elitism=1)
sec=(proc.time()-PTM)[3] # get seconds elapsed
bi=which.min(OEA$evaluations)
b=OEA$population[which.min(OEA$evaluations),]
cat("best fitness:",OEA$evaluations[1],"time elapsed:",sec,"\n")

# plot area of EA best solution: 
pdf("qa-ea-area.pdf",paper="special")
par(mar=c(0.0,0.0,0.0,0.0))
PEA=poly(Data[c(b,b[1]),])
plot(PEA,col="gray")
lines(Data[c(b,b[1]),],lwd=2)
dev.off()
