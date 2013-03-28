#potential primes:

rm(list=ls())
start<-2
range<-15000000
#make a list of integers up to 'range' that are not divisable by 2,3,or 5
a<-((((start:range)[c(F,T)])[c(F,T,T)]))[c(F,T,T,T,T,T,T,F,T,T)]
length(a)/range
#drop those that are divisable by 7
a<-c(a[2:12],((a[14:length(a)])[c(T,T,T,T,T,T,F,T,T,T,F)]))
length(a)/range
#drop divisable by 11
a<-c(a[2:26],((a[28:length(a)])[c(T,T,T,T,F,T,T,T,T,T,T,T,T,F)]))
1/(length(a)/range)

## that only works up to 11 because the pattern for 13 is complex and the vector allocation only works up to a range because of size
##Below is similar to the sieve of Eratosthenes

#How about this:
rm(list=ls())
prlist<-((read.table("fourth_list.txt"))[[1]])[1:1000]
start<-1
range<-max(prlist[500])^2
a<-(start:(start+range-1))

for (i in 2:500)
  {
  adrop<-(start:(start+range))*as.numeric(prlist[i])
  a<-setdiff(a,adrop)
  cat(i)          ; flush.console()
  }
a<-c(prlist[1:i],a[2:length(a)])

head(a)
