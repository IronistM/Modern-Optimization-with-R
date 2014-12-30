# two if else examples:
x=0; if(x>0) cat("positive\n") else if(x==0) cat("neutral\n") else cat("negative\n")
ifelse(xor(x,1),cat("TRUE\n"),cat("FALSE\n"))
print(switch(3,"a","b","c"))        # numeric switch example
x=1; while(x<3) { print(x); x=x+1;} # while example
for(i in 1:3) print(2*i)            # for example #1
for(i in c("a","b","c")) print(i)   # for example #2
for(i in 1:10) if(i%%3==0) print(i) # for example #3
                                  # character switch example:
var="sin";x=1:3;y=switch(var,cos=cos(x),sin=sin(x))
cat("the",var,"of",x,"is",round(y,digits=3),"\n")
