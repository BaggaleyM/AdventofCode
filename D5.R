#day 5 part 1
library(tidyverse)
rm(list=ls())#clear workspace 
input2<-read.csv("C:\\Users\\Michael\\OneDrive - University of Calgary\\Advent_of_code\\2021\\AOCT5.csv",header=FALSE)
value<-matrix(ncol=2,nrow=nrow(input2))
temp<-input2$V2
temp<-data.frame(temp)
y1x2<-separate_rows(temp,temp,sep = "->",convert=TRUE)
y1<-y1x2[seq(1,nrow(y1x2),2),]
x2<-y1x2[seq(2,nrow(y1x2),2),]

x1<-input2$V1
init<-data.frame(x1,y1)
colnames(init)<-c('x1','y1')
y2<-input2$V3
final<-data.frame(x2,y2)
colnames(final)<-c('x2','y2')

X<-cbind(x1,x2)
num_cols<-max(X)+1
Y<-cbind(y1,y2)
num_rows<-max(Y)+1

vert_lines<-which(init$x1==final$x2)
horz_lines<-which(init$y1==final$y2)

p_horz<-matrix(ncol=2,nrow=length(horz_lines))
p_horz[,1]<-init$x1[horz_lines]
p_horz[,2]<-init$y1[horz_lines]
p_vert<-matrix(ncol=2,nrow=length(vert_lines))
p_vert[,1]<-init$x1[vert_lines]
p_vert[,2]<-init$y1[vert_lines]
initial_pos<-rbind(p_horz,p_vert)+1
colnames(initial_pos)<-c('X','Y')

p_horz_f<-matrix(ncol=2,nrow=length(horz_lines))
p_horz_f[,1]<-final$x2[horz_lines]
p_horz_f[,2]<-final$y2[horz_lines]
p_vert_f<-matrix(ncol=2,nrow=length(vert_lines))
p_vert_f[,1]<-final$x2[vert_lines]
p_vert_f[,2]<-final$y2[vert_lines]
final_pos<-rbind(p_horz_f,p_vert_f)+1
colnames(final_pos)<-c('X','Y')

diagram<-matrix(ncol=num_cols,nrow=num_rows)
diagram[,]<-0
lines=sum(length(vert_lines),length(horz_lines))

for (i in 1:lines){
  diagram[initial_pos[i,2]:final_pos[i,2],initial_pos[i,1]:final_pos[i,1]]<-diagram[initial_pos[i,2]:final_pos[i,2],initial_pos[i,1]:final_pos[i,1]]+1
  #diagram[final_pos[i,2],final_pos[i,1]]<-diagram[final_pos[i,2],final_pos[i,1]]+1
}
string<-"lines that overlap="
overlap=length(which(diagram>=2))
print(paste(string,overlap))

#day 5 - part 2
library(tidyverse)
rm(list=ls())#clear workspace 
input2<-read.csv("C:\\Users\\Michael\\OneDrive - University of Calgary\\Advent_of_code\\2021\\AOC5.csv",header=FALSE)
value<-matrix(ncol=2,nrow=nrow(input2))
temp<-input2$V2
temp<-data.frame(temp)
y1x2<-separate_rows(temp,temp,sep = "->",convert=TRUE)
y1<-y1x2[seq(1,nrow(y1x2),2),]
x2<-y1x2[seq(2,nrow(y1x2),2),]

x1<-input2$V1
init<-data.frame(x1,y1)
colnames(init)<-c('x1','y1')
y2<-input2$V3
final<-data.frame(x2,y2)
colnames(final)<-c('x2','y2')

X<-cbind(x1,x2)
num_cols<-max(X)+1
Y<-cbind(y1,y2)
num_rows<-max(Y)+1

vert_lines<-which(init$x1==final$x2)
horz_lines<-which(init$y1==final$y2)
diag_lines<-which(init$x1!=final$x2 & init$y1!=final$y2)

init_diag<-data.frame(cbind(init$x1[diag_lines],init$y1[diag_lines]))+1
colnames(init_diag)<-c('X','Y')
final_diag<-data.frame(cbind(final$x2[diag_lines],final$y2[diag_lines]))+1
colnames(final_diag)<-c('X','Y')

p_horz<-matrix(ncol=2,nrow=length(horz_lines))
p_horz[,1]<-init$x1[horz_lines]
p_horz[,2]<-init$y1[horz_lines]
p_vert<-matrix(ncol=2,nrow=length(vert_lines))
p_vert[,1]<-init$x1[vert_lines]
p_vert[,2]<-init$y1[vert_lines]
initial_pos<-rbind(p_horz,p_vert)+1
colnames(initial_pos)<-c('X','Y')

p_horz_f<-matrix(ncol=2,nrow=length(horz_lines))
p_horz_f[,1]<-final$x2[horz_lines]
p_horz_f[,2]<-final$y2[horz_lines]
p_vert_f<-matrix(ncol=2,nrow=length(vert_lines))
p_vert_f[,1]<-final$x2[vert_lines]
p_vert_f[,2]<-final$y2[vert_lines]
final_pos<-rbind(p_horz_f,p_vert_f)+1
colnames(final_pos)<-c('X','Y')


diagram<-matrix(ncol=num_cols,nrow=num_rows)
diagram[,]<-0
#lines=nrow(input2)
lines=sum(length(vert_lines),length(horz_lines))

for (i in 1:lines){
  diagram[initial_pos[i,2]:final_pos[i,2],initial_pos[i,1]:final_pos[i,1]]<-diagram[initial_pos[i,2]:final_pos[i,2],initial_pos[i,1]:final_pos[i,1]]+1
  #
}

for (i in 1:length(diag_lines)){
  temp<-data.frame(cbind(init_diag[i,1]:final_diag[i,1],init_diag[i,2]:final_diag[i,2]))
  for (j in 1:nrow(temp)){
    diagram[temp[j,2],temp[j,1]]<-diagram[temp[j,2],temp[j,1]]+1
  }
}

string<-"lines that overlap="
overlap=length(which(diagram>=2))
print(paste(string,overlap))