#Ulam spiral
rm(list=ls())
setwd("H:/Personal/criptic")
size<-801 #this needs to be an odd number less that the square root of your maximum prime
lista <- (as.numeric(!is.na(match((1:size^2),((read.table("fourth_list.txt"))$prlist[1:ceiling(((size^2)/log((size^2)))*1.5)])))))[1:size^2]
a<-1
b<-1
c<-1
d<-c(1:size^2)
for (i in 2:(ceiling(size/2))) {
  a<-append(a,rep(i, (i-1)*8))    
  b<-append(b,seq(from=1,to=((i-1)*8)))
  c<-append(c,rep(((i*2)-1), ((i-1)*8)))
}
a<-data.frame(cbind(d,a,b,c,lista))
names(a)<-c("move_nr","boxid", "id", "boxsize", "prime")
#tail(a)

r<-ceiling(size/2)
c<-ceiling(size/2)

mat1<-matrix(NA,nrow=size,ncol=size)
mat1[r,c]<-a$prime[1]
for (i in 2:size^2) {
id<-a$id[i]
boxsize<-a$boxsize[i]
  if (id==1){
    r<-(r-1)}
  else if (id<boxsize & id!=1){
    c<-c+1}
  else if (id<(2*boxsize-1) & id>=boxsize){
    r<-r+1}
  else if (id<(3*boxsize-2) & (id>=2*boxsize-1)){
    c<-c-1}
  else if (id<(4*boxsize-3) & (id>=3*boxsize-2)){
    r<-r-1}
  mat1[r,c]<-a$prime[i]
  #cat(paste(r,c,"   "))  ; flush.console()
}

require(jpeg)
writeJPEG(1-mat1, target="H:/Personal/criptic/ulam_spiral_101_invert.jpeg", quality=1)

b<-data.frame(cbind(1:length(mat1),1:dim(mat1)[1],rep(1:dim(mat1)[1], each=dim(mat1)[2]),NA))
names(b)<-c("id","x", "y", "status")
for (i in 1:length(mat1)) {
b$status[i]<-mat1[b$y[i],b$x[i]] 
}

b_all<-as.points(b$x, b$y)
b_pos<-as.points(b$x[b$status==1], b$y[b$status==1])
b_neg<-as.points(b$x[b$status==0], b$y[b$status==0])

smooth_pic<-kernrat(b_pos, b_neg, bboxx(bbox(b_all)), h1=10, h2=10, nx=dim(mat1)[1], ny=dim(mat1)[2], kernel='quartic')
image(smooth_pic, col=gray.colors(100, start = 0.9, end = 0.2, gamma = 2.2))



