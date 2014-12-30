x=1:10;print(x)
print(x>=3&x<8)                 # select some elements
I=which(x>=3&x<8);print(I)      # indexes of selection
d=data.frame(x=1:4,f=factor(c(rep("a",2),rep("b",2))))
print(d)
print(d[d$x<2|d$f=="b",])                # select rows
