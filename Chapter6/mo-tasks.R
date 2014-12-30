### mo-tasks.R file ###

# binary multi-optimization goal:
sumbin=function(x) (sum(x))
intbin=function(x) sum(2^(which(rev(x==1))-1))
maxsin=function(x) # max sin (explained in Chapter 3)
{ D=length(x);x=intbin(x)
  return(sin(pi*(as.numeric(x))/(2^D))) }

# integer multi-optimization goal:
profit=function(x)    # x - a vector of prices
{ x=round(x,digits=0) # convert x into integer
  s=sales(x)          # get the expected sales
  c=cost(s)           # get the expected cost
  profit=sum(s*x-c)   # compute the profit
  return(profit) 
}
cost=function(units,A=100,cpu=35-5*(1:length(units)))
{ return(A+cpu*units) }
sales=function(x,A=1000,B=200,C=141,
               m=seq(2,length.out=length(x),by=-0.25))
{ return(round(m*(A/log(x+B)-C),digits=0))}
produced=function(x) sum(sales(round(x)))

# real value FES1 benchmark:
fes1=function(x)
{ D=length(x);f1=0;f2=0
  for(i in 1:D)
    { f1=f1+abs(x[i]-exp((i/D)^2)/3)^0.5
      f2=f2+(x[i]-0.5*cos(10*pi/D)-0.5)^2
    }
  return(c(f1,f2)) 
}
