library(tidyverse)
library(ggplot2)

#alternative solution
rm(list=ls())#clear workspace 
d <- read.csv("C:\\Users\\Michael\\OneDrive - University of Calgary\\Advent_of_code\\2021\\AOC3.csv", fileEncoding="UTF-8-BOM",header=F,colClasses=c("character"))#numeric data cannot have leading or trailing zeros
length=length(d$V1)
str_length=nchar(d$V1)
str_length=str_length[1]
value<-matrix(ncol=str_length,nrow=length)
for (i in 1:str_length){
  value[,i]=as.numeric(substr(d$V1,i,i))
}
value.df=data.frame(value)

#OPR
val<-value.df
cnt=1
while(nrow(val)>2){
  sum(val[,cnt])
  if (sum(val[,cnt])>=(nrow(val)/2)){
    keep=1
    val<-val[val[,cnt] %in% keep,]
    cnt=cnt+1
  }else{
    keep=0
    val<-val[val[,cnt] %in% keep,]
    cnt=cnt+1
  }
}
sum(val[,cnt])
if(sum(val[,ncol(val)])!=0){
  keep=1
  val<-val[val[,ncol(val)] %in% keep,]
}else{
  keep=0
  val<-val[val[,ncol(val)] %in% keep,]
}

val
ogr=paste(val,collapse="")
ogr_int=strtoi(ogr,base=2)
ogr_int

#CSR
val<-value.df
cnt=1
while(nrow(val)>2){
  sum(val[,cnt])
  if (sum(val[,cnt])>(nrow(val)/2)){
    keep=0
    val<-val[val[,cnt] %in% keep,]
    cnt=cnt+1
  }else{
    keep=1
    val<-val[val[,cnt] %in% keep,]
    cnt=cnt+1
  }
}
sum(val[,cnt])
if(sum(val[,ncol(val)])==2){
  keep=1
  val<-val[val[,ncol(val)] %in% keep,]
}else{
  keep=0
  val<-val[val[,ncol(val)] %in% keep,]
}

csr=paste(val,collapse="")
csr_int=strtoi(csr,base=2)
csr_int
lsr=ogr_int*csr_int
lsr
