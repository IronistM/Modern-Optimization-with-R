source("functions.R") # load the code
cat("class of profit is:",class(profit),"\n") # function
x=c(414.1,404.2,408.3,413.2,395.0)
y=profit(x); cat("maximum profit:",y,"\n")
cat("x is not changed:",x,"\n")
cat("cost(x=",x,")=",cost(x),"\n") 
cat("sales(x=",x,")=",sales(round(x)),"\n") 
x=c(414,404); # sales for 2 bags:
cat("sales(x=",x,")=",sales(x),"\n")
cat("sales(x,A=1000,m=c(2,1.75))=",sales(x,1000,m=c(2,1.75)),"\n")
# show 3! :
x=3; cat("fact(",x,")=",fact(x),"\n")
