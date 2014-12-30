### lg-ga.R file ###

# lexicographic comparison of several solutions:
#    x - is a matrix with several objectives at each column
#        and each row is related with a solution
lexibest=function(x) # assumes LEXI is defined
{
 size=nrow(x); m=ncol(x)
 candidates=1:size
 stop=FALSE; i=1
 while(!stop)
   {
    F=x[candidates,i] # i-th goal
    minFID=which.min(F) # minimization goal is assumed
    minF=F[minFID]
    # compute tolerance value
    if(minF>-1 && minF<1) tolerance=LEXI[i] 
    else tolerance=abs(LEXI[i]*minF)
    I=which((F-minF)<=tolerance)
    if(length(I)>0) # at least one candidate
      candidates=candidates[I] # update candidates
    else stop=TRUE
    if(!stop && i==m) stop=TRUE
    else i=i+1
   }
 if(length(candidates)>1) 
   { # return highest priority goal if no clear winner:
     stop=FALSE; i=1
     while(!stop)
       {
        minF=min(x[candidates,i])
        I=which(x[candidates,i]==minF)
        candidates=candidates[I]
        if(length(candidates)==1||i==m) stop=TRUE
        else i=i+1
       }
     # remove (any) extra duplicate individuals:
     candidates=candidates[1]
   }
 # return lexibest:
 return(candidates)
}

# compare k randomly selected solutions from Population: 
#    returns n best indexes of Population (decreasing order)
#    m is the number of objectives
tournament=function(Population,evalFunc,k,n,m=2)
{
 popSize=nrow(Population)
 PID=sample(1:popSize,k) # select k random tournament solutions
 E=matrix(nrow=k,ncol=m) # evaluations of tournament solutions
 for(i in 1:k) # evaluate tournament 
    E[i,]=evalFunc(Population[PID[i],])

 # return best n individuals:
 B=lexibest(E); i=1; res=PID[B] # best individual
 while(i<n) # other best individuals
   { 
     E=E[-B,];PID=PID[-B] # all except B
     if(is.matrix(E)) B=lexibest(E)
     else B=1 # only 1 row
     res=c(res,PID[B])
     i=i+1
   }
 return(res)
}

# lexicographic adapted version of rbga.bin:
#    this function is almost identical to rbga.bin except that
#    the code was simplified and a lexicographic tournament is used
#    instead of roulette wheel selection
lrbga.bin=function(size=10, suggestions=NULL, popSize=200, 
                   iters=100, mutationChance=NA, elitism=NA, 
                   zeroToOneRatio=10,evalFunc=NULL) 
{
 vars=size
 if(is.na(mutationChance)) { mutationChance=1/(vars + 1) }
 if(is.na(elitism)) { elitism=floor(popSize/5)}
 if(!is.null(suggestions))
   {
    population=matrix(nrow=popSize, ncol=vars)
    suggestionCount=dim(suggestions)[1]
    for(i in 1:suggestionCount) 
      population[i, ]=suggestions[i, ]
    for(child in (suggestionCount + 1):popSize) 
      {
       population[child, ]=sample(c(rep(0, zeroToOneRatio),1),vars,rep=TRUE)
       while(sum(population[child, ])==0)
          population[child, ]=sample(c(rep(0, zeroToOneRatio),1),vars,rep=TRUE)
      }
   }
 else
   {
    population=matrix(nrow=popSize, ncol=vars)
    for(child in 1:popSize) 
      {
       population[child,]=sample(c(rep(0, zeroToOneRatio),1),vars,rep=TRUE)
       while (sum(population[child, ]) == 0) 
         population[child, ]=sample(c(rep(0, zeroToOneRatio),1),vars,rep=TRUE)
      }
   }
 # main GA cycle:
 for(iter in 1:iters) 
   {
    newPopulation=matrix(nrow=popSize, ncol=vars)
    if(elitism>0) # applying elitism:
      { 
       elitismID=tournament(population,evalFunc,k=popSize,n=elitism)
       newPopulation[1:elitism,]=population[elitismID,]
      }
    #  applying crossover:
    for(child in (elitism + 1):popSize) 
      {
       ### very new code inserted here : ###
       pID1=tournament(population,evalFunc=evalFunc,k=2,n=1)
       pID2=tournament(population,evalFunc=evalFunc,k=2,n=1)
       parents=population[c(pID1,pID2),]
       ### end of very new code          ###
       crossOverPoint=sample(0:vars, 1)
       if(crossOverPoint == 0)
         newPopulation[child,]=parents[2,]
       else if(crossOverPoint == vars) 
         newPopulation[child, ]=parents[1, ]
       else 
         {
          newPopulation[child,]=c(parents[1,][1:crossOverPoint],parents[2,][(crossOverPoint+1):vars])
          while(sum(newPopulation[child,])==0) 
            newPopulation[child, ]=sample(c(rep(0,zeroToOneRatio),1),vars,rep=TRUE)
         }
      }
    population=newPopulation # store new population
    if(mutationChance>0) # applying mutations:
      {
       mutationCount=0
       for(object in (elitism+1):popSize) 
         {
          for(var in 1:vars)
            {
             if(runif(1)< mutationChance) 
               {
                population[object, var]=sample(c(rep(0,zeroToOneRatio),1),1)
                mutationCount=mutationCount+1
               }
            }
         }
      }
   } # end of GA main cycle
 result=list(type="binary chromosome",size=size,popSize=popSize, 
        iters=iters,suggestions=suggestions,
        population=population,elitism=elitism,
        mutationChance=mutationChance)
 return(result)
}
