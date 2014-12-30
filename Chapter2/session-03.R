f=factor(c("a","a","b","b","c")); print(f) # create factor
f[1]="c"; print(f)                         # change factor
print(levels(f))         # show domain levels: "a" "b" "c" 
print(summary(f))                   # show a summary of y
# pdf("f-barplot.pdf")
plot(f)                                   # show y barplot
#dev.off()
x=c(1.1,2.3,-1,4,2e-2) # creates vector x
summary(x)             # show summary of x
print(x)               # show x
str(x)                 # show x structure
length(x)              # show the length of x
x[2]                   # show second element of x
x[2:3]=(2:3)*1.1       # change 2nd and 3rd elements
x[length(x)]=5         # change last element to 5
print(x)               # show x
print(x>3)             # show which x elements > 3
print(which(x>3))      # show indexes of x>3 condition
names(x)=c("1st","2nd","3rd","4th","5th") # change names of x
print(x)               # show x
print(mean(x))         # show the average of x
print(summary(x))      # show a summary of x
y=vector(length=5); print(y)      # FALSE, FALSE, ..., FALSE
y[]=1; print(y)                   # all elements set to 1
y[c(1,3,5)]=2; print(y)           # 2,1,2,1,2
# fancier plot of y:
plot(y,type="b",lwd=3,col="gray",pch=19,panel.first=grid(5,5))
