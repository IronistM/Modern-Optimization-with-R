x=sample(1:10,5,replace=TRUE)  # 5 random samples from 1 to 10 with replacement
print(x)                       # show x
print(min(x))                  # show min of x
print(which.min(x))            # show index of x that contains min
print(sort(x,decreasing=TRUE)) # show x in decreasing order
y=seq(0,20,by=2); print(y)     # y = 0, 2, ..., 20
print(y[x])                    # show y[x]
print(y[-x])            # - means indexes of y without x values
x=runif(5,0.0,10.0);print(x)   # 5 uniform samples from 0 to 10
y=rnorm(5,10.0,1.0);print(y)   # 5 normal samples with mean 10 and std 1.0
t.test(x,y)                    # t-student paired test
