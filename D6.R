#day 6 part 1
library(tidyverse)
rm(list=ls())#clear workspace 
#input<-read.csv("C:\\Users\\Michael\\OneDrive - University of Calgary\\Advent_of_code\\2021\\AOCT6.csv",header=FALSE)

#input<-read.delim("C:\\Users\\Michael\\OneDrive - University of Calgary\\Advent_of_code\\2021\\AOCT6.txt", header=FALSE)

input<-read.table("C:\\Users\\Michael\\OneDrive - University of Calgary\\Advent_of_code\\2021\\AOC6.txt", header=FALSE, sep= ":")
fish<-data.frame(input$V1)
lastday=nrow(fish)
start_fish<-str_split(fish[lastday,],pattern =",")
#start_fish<-str_split(fish[1,],pattern =",")
names(start_fish)<-c("v1")
nfish<-length(start_fish$v1)
value<-matrix(ncol=1,nrow=nfish)
for (i in 1:nfish){
  value[i,]<-as.numeric(start_fish$"v1"[i])
}
value<-data.frame(value)

days<-256
for (i in 1:days){
  numfish<-nrow(value)
  for (j in 1:numfish){
    if (value[j,]>0){
      value[j,]<-value[j,]-1
    } else{
      value[j,]<-6
      end<-nrow(value)
      value[end+1,]<-8
        }
  }
}
end_time <- Sys.time()
end_time - start_time

num<-c(1,2,3,4,5,6,7,8)

start_time <- Sys.time()
for (i in 1:days){
  zero<-seq(length(value$value))[value[,] %in% 0]
  hits<-seq(length(value$value))[value[,] %in% num]
  if (length(zero) > 0){
    numzero<-length(zero)
    eights<-matrix(ncol=1,nrow=numzero)
    eights[,]<-8
    eights<-data.frame(eights)
    names(eights)<-c("value")
    value<-rbind(value,eights)
  }
  value[zero,]<-6
  value[hits,]<-value[hits,]-1
  
  
}
string<-"lanternfish="
lf<-length(value$value)
print(paste(string,lf))
end_time <- Sys.time()
end_time - start_time


start_time <- Sys.time()

string<-"lanternfish="
numlf<-sum(lf)
print(paste(string,lf))
end_time <- Sys.time()
end_time - start_time

#part 2
rm(list=ls())#clear workspace 
input<-read.table("C:\\Users\\Michael\\OneDrive - University of Calgary\\Advent_of_code\\2021\\AOC6.txt", header=FALSE, sep= ":")
fish<-data.frame(input$V1)
lastday=nrow(fish)
start_fish<-str_split(fish[lastday,],pattern =",")
#start_fish<-str_split(fish[1,],pattern =",")
names(start_fish)<-c("v1")
nfish<-length(start_fish$v1)
value<-matrix(ncol=1,nrow=nfish)
for (i in 1:nfish){
  value[i,]<-as.numeric(start_fish$"v1"[i])
}
value<-data.frame(value)

days<-256
lf<-data.frame(v0=length(seq(length(value$value))[value[,]==0]),
               v1=length(seq(length(value$value))[value[,]==1]),
               v2=length(seq(length(value$value))[value[,]==2]),
               v3=length(seq(length(value$value))[value[,]==3]),
               v4=length(seq(length(value$value))[value[,]==4]),
               v5=length(seq(length(value$value))[value[,]==5]),
               v6=length(seq(length(value$value))[value[,]==6]),
               v7=length(seq(length(value$value))[value[,]==7]),
               v8=length(seq(length(value$value))[value[,]==8]))

for (i in 1:days){
  zeroes<-lf$v0
  lf$v0 <- lf$v1
  lf$v1 <- lf$v2
  lf$v2 <- lf$v3
  lf$v3 <- lf$v4
  lf$v4 <- lf$v5
  lf$v5 <- lf$v6
  lf$v6 <- lf$v7+zeroes
  lf$v7 <- lf$v8
  lf$v8 <- zeroes
}
string<-"lanternfish="
numlf<-Reduce('+',lf)
format(numlf, scientific = FALSE)
print(paste(string,numlf))
