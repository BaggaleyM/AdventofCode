#Day 7 - part 1
library(tidyverse)
rm(list=ls())#clear workspace 

input<-read.table("C:\\Users\\Michael\\OneDrive - University of Calgary\\Advent_of_code\\2021\\AOC7.txt", header=FALSE, sep= ",")
df<-as.data.frame(t(input))

crabs_sorted<-sort(df$V1,decreasing = FALSE)
maxpos<-max(crabs_sorted)
unique_pos<-data.frame(unique(crabs_sorted))
cost<-matrix(ncol=maxpos,nrow=nrow(df))
poss_pos<-c(0:maxpos)
#minimize the summed displacement of crabs
for (i in 1:nrow(df)){
  for (j in 1:maxpos){
    cost[i,j]<-abs(df[i,]-poss_pos[j])
  }
}

sumcost<-colSums(cost)
tcost<-matrix(ncol = maxpos,nrow = nrow(df))

for (i in 1:length(sumcost)){
  for (j in 1:nrow(cost)){
    tcost[j,i]<-sum(1:cost[j,i])
  }
}

sumtcost<-colSums(tcost)
tc<-which.min(sumtcost)
fuelcost<-sumtcost[tc]
pos<-which.min(colSums(cost))
fuel<-sumcost[pos]
string<-"fuel used = "
print(paste(string,fuelcost))
