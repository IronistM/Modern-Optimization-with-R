x=list(a=1:3,b="hello!")          # x is a list
save(x,file="x.Rdata",ascii=TRUE) # save into working directory
rm(x)                             # remove an object
print(x)                          # gives an error
load("x.Rdata")                   # x now exists!
print(x)                          # show x
t=readLines("x.Rdata")            # read all text file
cat("first line:",t[1],"\n")      # show 1st line
cat("first line:",readLines("x.Rdata",n=1),"\n") 
# write a text file using writeLines:
conn=file("demo.txt")            # create a connection
writeLines("hello!", conn)        # write something
close(conn)                       # close connection
# write a text file using sink:
sink("demo2.txt")                 # divert output
cat("hello!\n")                   # write something
sink()                            # stop sink
