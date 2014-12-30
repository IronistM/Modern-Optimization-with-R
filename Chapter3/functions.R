### functions.R file ###
# compute the bag factory profit for x:
#    x - a vector of prices
profit=function(x)    # x - a vector of prices
{ x=round(x,digits=0) # convert x into integer
  s=sales(x)          # get the expected sales
  c=cost(s)           # get the expected cost
  profit=sum(s*x-c)   # compute the profit
  return(profit) 
# local variables x, s, c and profit are lost from here
}

# compute the cost for producing units:
#    units - number of units produced
#    A - fixed cost, cpu - cost per unit
cost=function(units,A=100,cpu=35-5*(1:length(units)))
{ return(A+cpu*units) }

# compute the estimated sales for x:
#    x - a vector of prices, m - marketing effort
#    A, B, C - constants of the estimated function
sales=function(x,A=1000,B=200,C=141,
               m=seq(2,length.out=length(x),by=-0.25))
{ return(round(m*(A/log(x+B)-C),digits=0))}

# example of a simple recursive function:
fact=function(x=0) # x - integer number 
{ if(x==0) return(1) else return(x*fact(x-1))}
