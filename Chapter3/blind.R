### blind.R file ###

# full bind search method
#    search - matrix with solutions x D
#    FUN - evaluation function
#    type - "min" or "max"
#    ... - extra parameters for FUN
fsearch=function(search,FUN,type="min",...)
{
 x=apply(search,1,FUN,...) # run FUN over all search rows
 ib=switch(type,min=which.min(x),max=which.max(x))
 return(list(index=ib,sol=search[ib,],eval=x[ib]))
}

# depth-first full search method
#    l - level of the tree
#    b - branch of the tree
#    domain - vector list of size D with domain values
#    FUN - eval function
#    type - "min" or "max"
#    D - dimension (number of variables)
#    x - current solution vector
#    bcur - current best sol
#    ... - extra parameters for FUN
dfsearch=function(l=1,b=1,domain,FUN,type="min",D=length(domain),
                  x=rep(NA,D),
                  bcur=switch(type,min=list(sol=NULL,eval=Inf),
                                   max=list(sol=NULL,eval=-Inf)),
                  ...)
{ if((l-1)==D) # "leave" with solution x to be tested:
     { f=FUN(x,...);fb=bcur$eval
       ib=switch(type,min=which.min(c(fb,f)),
                      max=which.max(c(fb,f)))
       if(ib==1) return (bcur) else return(list(sol=x,eval=f))
     }
  else # go through sub branches
     { for(j in 1:length(domain[[l]]))
          { x[l]=domain[[l]][j]
            bcur=dfsearch(l+1,j,domain,FUN,type,D=D,
                          x=x,bcur=bcur,...)
          }
       return(bcur)
     }
}
