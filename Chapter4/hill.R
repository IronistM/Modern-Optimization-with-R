### hill.R file ###

# pure hill climbing:
#    par - initial solution
#    fn - evaluation function
#    change - function to generate the next candidate 
#    lower - vector with lowest values for each dimension
#    upper - vector with highest values for each dimension
#    control - list with stopping and monitoring method:
#       $maxit - maximum number of iterations
#       $REPORT - frequency of monitoring information
#    type - "min" or "max"
#    ... - extra parameters for FUN
hclimbing=function(par,fn,change,lower,upper,control,
                   type="min",...)
{ fpar=fn(par,...)
  for(i in 1:control$maxit) 
     { 
      par1=change(par,lower,upper) 
      fpar1=fn(par1,...)
      if(control$REPORT>0 &&(i==1||i%%control$REPORT==0)) 
         cat("i:",i,"s:",par,"f:",fpar,"s'",par1,"f:",fpar1,"\n")
      if(   (type=="min" && fpar1<fpar) 
         || (type=="max" && fpar1>fpar)) { par=par1;fpar=fpar1 }
     }
  if(control$REPORT>=1) cat("best:",par,"f:",fpar,"\n")
  return(list(sol=par,eval=fpar))
}

# slight random change of vector par:
#    par - initial solution
#    lower - vector with lowest values for each dimension
#    upper - vector with highest values for each dimension
#    dist - random distribution function
#    round - use integer (TRUE) or continuous (FALSE) search
#    ... - extra parameters for dist
#    examples: dist=rnorm, mean=0, sd=1; dist=runif, min=0,max=1
hchange=function(par,lower,upper,dist,round=TRUE,...)
{ D=length(par) # dimension
  step=dist(D,...) # slight step
  if(round) step=round(step) 
  par1=par+step
  # return par1 within [lower,upper]:
  return(ifelse(par1<lower,lower,ifelse(par1>upper,upper,par1)))
}
