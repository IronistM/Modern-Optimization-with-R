### test-mc.R file ###

source("blind.R") # load the blind search methods
source("montecarlo.R") # load the monte carlo method 
source("functions.R") # load the profit function

N=10000 # set the number of samples
cat("monte carlo search (N:",N,")\n")

# bag prices
cat("bag prices:")
S=mcsearch(N,rep(1,5),rep(1000,5),profit,"max") 
cat("s:",S$sol,"f:",S$eval,"\n")

# real-value functions: sphere and rastrigin:
sphere=function(x) sum(x^2)
rastrigin=function(x) 10*length(x)+sum(x^2-10*cos(2*pi*x))

D=c(2,30)
label="sphere"
for(i in 1:length(D))
   { S=mcsearch(N,rep(-5.2,D[i]),rep(5.2,D[i]),sphere,"min") 
     cat(label,"D:",D[i],"s:",S$sol[1:2],"f:",S$eval,"\n")
   }
label="rastrigin"
for(i in 1:length(D))
   { S=mcsearch(N,rep(-5.2,D[i]),rep(5.2,D[i]),rastrigin,"min") 
     cat(label,"D:",D[i],"s:",S$sol[1:2],"f:",S$eval,"\n")
   }
