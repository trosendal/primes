rm(list=ls())
ast<-Sys.time()
searchlength<-100000000
setwd("H:/Personal/criptic")
prlist<-(read.table("fourth_list.txt"))[[1]]
temp<-(max(prlist))
time<-(read.table("fourth_list.txt"))[[2]]
#prlist<-c(1,2)
#time<-c(0,0)
a<-prlist[length(prlist)]+searchlength
for (i in (prlist[length(prlist)]+1):a) {
  if(((i/1000000)-floor(i/1000000))==0){
    cat(paste("SAVING TO DISK        ")); flush.console()
    list_of_primes<-as.data.frame(cbind(prlist,time))
    write.table((list_of_primes),"fourth_list.txt", sep=" ") 
  
  }
  testvalue<-0
  start_time<-Sys.time()
  for(j in 2:(length(prlist))) {
    if(j<=(i^0.5+1)){
    c<-(i/prlist[j])-floor(i/prlist[j])
    if(c!=0){
      next
    }else {
      testvalue<-1
      break
    }
  }
  else{break}  
  }
  if(testvalue!=0){
    next
  }else {
    prlist[(length(prlist)+1)]<-i
    stop_time<-Sys.time()
    diff_time<-stop_time-start_time[[1]]
    time[(length(prlist))]<-diff_time
    cat(paste(round((100*((i-temp)/searchlength)),1),"%                    "))          ; flush.console()
  }  
  
}
aspt<-Sys.time()
aspt-ast
(aspt-ast)/searchlength
list_of_primes<-as.data.frame(cbind(prlist,time))
write.table((list_of_primes),"fourth_list.txt", sep=" ")

tail(list_of_primes,5)
length(list_of_primes$prlist[which((max(list_of_primes$prlist))^0.5>list_of_primes$prlist)])

