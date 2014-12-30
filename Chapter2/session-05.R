m=matrix(ncol=3,nrow=2); m[,]=0; print(m) # 3x2 matrix
m[1,]=1:3; print(m)                       # change 1st row
m[,3]=1:2; print(m)                       # change 3rd column
m[2,1]=3; print(m)                        # change m[2,1]
print(nrow(m))                            # number of rows
print(ncol(m))                            # number of columns
m[nrow(m),ncol(m)]=5; print(m)            # change last element
m[nrow(m)-1,ncol(m)-1]=4; print(m)        # change m[1,2]
print(max(m))                             # show maximum of m
m=sqrt(m); print(m)                       # change m
m[1,]=c(1,1,2013); m[2,]=c(2,2,2013)      # change m
d=data.frame(m)                           # create data.frame
names(d)=c("day","month","year")          # change names
d[1,]=c(2,1,2013); print(d)               # change 1st row
d$day[2]=3; print(d)                      # change d[1,2]
d=rbind(d,c(4,3,2014)); print(d)          # add row to d
# change 2nd column of d to factor, same as d[,2]=factor(...
d$month=factor(c("Jan","Feb","Mar")) 
print(summary(d))                         # summary of d
