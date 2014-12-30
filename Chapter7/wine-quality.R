### wine-quality.R file ###

library(rminer) # load rminer package
library(kernlab) # load svm functions used by rminer
library(mco) # load mco package

# load wine quality dataset directly from UCI repository:
file="http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"
d=read.table(file=file,sep=";",header=TRUE) 

# convert the output variable into 3 classes of wine:
# "poor_or_average" <- 3,4,5 or 6; 
# "good_or_excellent" <- 7, 8 or 9
d$quality=cut(d$quality,c(1,6,10),
              c("poor_or_average","good_or_excellent"))
output=ncol(d) # output target index (last column)
maxinputs=output-1 # number of maximum inputs

# to speed up the demonstration, select a smaller sample of data:
n=nrow(d) # total number of samples
ns=round(n*0.25) # select a quarter of the samples
set.seed(12345) # for replicability
ALL=sample(1:n,ns) # contains 25% of the index samples
# show a summary of the wine quality dataset (25%):
print(summary(d[ALL,]))
cat("output class distribuition (25% samples):\n")
print(table(d[ALL,]$quality)) # show distribution of classes

# holdout split:
# select training data (for fitting the model), 70%; and 
# test data (for estimating generalization capabilities), 30%.
H=holdout(d[ALL,]$quality,ratio=0.7)
cat("nr. training samples:",length(H$tr),"\n")
cat("nr. test samples:",length(H$ts),"\n")

# evaluation function:
# x is in the form c(Gamma,C,b1,b2,...,b11)
eval=function(x)
{ n=length(x)
  gamma=2^x[1]
  C=2^x[2]
  features=round(x[3:n])
  inputs=which(features==1)
  attributes=c(inputs,output)
  # divert console:
  # sink is used to avoid kernlab ksvm messages in a few cases
  sink(file=textConnection("rval","w",local = TRUE))
  M=mining(quality~.,d[H$tr,attributes],method=c("kfold",3),model="svm",search=gamma,mpar=c(C,NA))
  sink(NULL) # restores console
  # AUC for the internal 3-fold cross-validation:
  auc=as.numeric(mmetric(M,metric="AUC")) 
  auc1=1-auc # transform auc maximization into minimization goal
  return(c(auc1,length(inputs)))
}

# NSGAII multi-objective optimization:
cat("NSGAII optimization:\n")
m=2 # two objectives: AUC and number of features
lower=c(-15,-5,rep(0,maxinputs))
upper=c(3,15,rep(1,maxinputs))
PTM=proc.time() # start clock
G=nsga2(fn=eval,idim=length(lower),odim=m,lower.bounds=lower,upper.bounds=upper,popsize=12,generations=10)
sec=(proc.time()-PTM)[3] # get seconds elapsed
cat("time elapsed:",sec,"\n")

# show the Pareto front:
I=which(G$pareto.optimal)
for(i in I) 
  { x=G$par[i,] 
    n=length(x)
    gamma=2^x[1]
    C=2^x[2]
    features=round(x[3:n])
    inputs=which(features==1)
    cat("gamma:",gamma,"C:",C,"features:",inputs,"; f=(",
        1-G$value[i,1],G$value[i,2],")\n",sep=" ")
  }

# create PDF showing the Pareto front:
pdf(file="nsga-wine.pdf",paper="special",height=5,width=5)
par(mar=c(4.0,4.0,0.1,0.1)) 
SI=sort.int(G$value[I,1],index.return=TRUE)
plot(1-G$value[SI$ix,1],G$value[SI$ix,2],xlab="AUC",ylab="nr. features",type="b",lwd=2)
dev.off()

# selection of the SVM model with 4 inputs:
x=G$par[I[7],]
gamma=2^x[1]
C=2^x[2]
features=round(x[3:n])
inputs=which(features==1)
attributes=c(inputs,output)
# fit a SVM with the optimized parameters:
cat("fit SVM with nr features:",length(inputs),"nr samples:",length(H$tr),"gamma:",gamma,"C:",C,"\n")
cat("inputs:",names(d)[inputs],"\n")
M=fit(quality~.,d[H$tr,attributes],model="svm",
      search=gamma,mpar=c(C,NA))
# get SVM predictions for unseen data:
P=predict(M,d[H$ts,attributes])
# create PDF showing the ROC curve for unseen data:
auc=mmetric(d[H$ts,]$quality,P,metric="AUC")
main=paste("ROC curve for test data",
           " (AUC=",round(auc,digits=2),")",sep="")
mgraph(d[H$ts,]$quality,P,graph="ROC",PDF="roc-wine",main=main,baseline=TRUE,Grid=10,leg="SVM")
