library(multicore)        # load the package
x1=1:5;x2=5:10     # create 2 objects
p1=parallel(factorial(x1)) # run in parallel
p2=parallel(factorial(x2)) # run in parallel
collect(list(p1,p2))       # collect results
