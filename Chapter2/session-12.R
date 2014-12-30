# create and write a simple data.frame:
d=data.frame(day=1:2,mon=factor(c("Jan","Feb")),year=c(12,13))
print(d)
write.table(d,file="demo.csv",row.names=FALSE,sep=";")
# read the created data.frame:
d2=read.table("demo.csv",header=TRUE,sep=";")
print(d2)
# read white wine quality dataset from UCI repository: 
library(RCurl)
URL="http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"
wine=getURL(URL)
write(wine,file="winequality-white.csv") # write to working directory
w=read.table("winequality-white.csv",header=TRUE,sep=";") # read file
cat("wine data (",nrow(w),"x",ncol(w),")\n")       # show nrow x ncol
