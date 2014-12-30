l=list(a="hello",b=1:3) # list with 2 components 
print(summary(l))       # summary of l
print(l)                # show l
l$b=l$b^2+1;print(l)    # change b to (b*b)+1
v=vector("list",3)  # vector list
v[[1]]=1:3          # change 1st element of v
v[[2]]=2            # change 2nd element of v
v[[3]]=l            # change 3rd element of v
print(v)            # show v
print(length(v))    # length of v
