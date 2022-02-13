library(tidyverse)
rm(list=ls())#clear workspace 
input2<-read.csv("C:\\Users\\Michael\\OneDrive - University of Calgary\\Advent_of_code\\2021\\AOCT5.csv",header=FALSE)
value<-matrix(ncol=2,nrow=nrow(input2))
temp<-input2$V2
temp<-data.frame(temp)
y1x2<-separate_rows(temp,temp,sep = "->")
y1<-y1x2[seq(1,nrow(y1x2),2),]
x2<-y1x2[seq(2,nrow(y1x2),2),]

for (i in 1:nrow(input2)){
  value[i,1]<-as.numeric(substr(input2$V2[i],1,3))
  value[i,2]<-as.numeric(substr(input2$V2[i],8,10))
}
value.df=data.frame(value)

x1<-input2$V1
#y1<-value.df$X1
init<-data.frame(x1,y1)
colnames(init)<-c('x1','y1')
#x2<-value.df$X2
y2<-input2$V3
final<-data.frame(x2,y2)
colnames(final)<-c('x2','y2')


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

#not used--------------
p1x<-init$x1[horz_lines]
p2x<-final$x2[horz_lines]
px<-data.frame(p1x,p2x)
p1y<-init$y1[vert_lines]
p2y<-final$y2[vert_lines]
py<-data.frame(p1y,p2y)
#-------------------
diagram<-matrix(ncol=10,nrow=nrow(init))
diagram[,]<-0
lines=sum(length(vert_lines),length(horz_lines))

for (i in 1:lines){
  diagram[initial_pos[i,2]:final_pos[i,2],initial_pos[i,1]:final_pos[i,1]]<-diagram[initial_pos[i,2]:final_pos[i,2],initial_pos[i,1]:final_pos[i,1]]+1
  #diagram[final_pos[i,2],final_pos[i,1]]<-diagram[final_pos[i,2],final_pos[i,1]]+1
}
string<-"lines that overlap="
overlap=length(which(diagram==2))
print(paste(string,overlap))

#notused-----------------------
#sq2fill<-abs(initial_pos-final_pos)
sq2fill<-final_pos-initial_pos
change<-which(sq2fill!=0)
tochange<-length(change)
sq2fill[change]<-sq2fill[change]-1

for (i in 1:length(sq2fill[,1])){
  p<-sq2fill[i,]
  if (sq2fill[i,1]==0){
    diagram[initial_pos[i,2]:sq2fill[i,2],initial_pos[i,1]]<-diagram[initial_pos[i,2]:sq2fill[i,2],initial_pos[i,1]]+1
    
  } else if (sq2fill[i,2]==0){
    diagram[initial_pos[i,2],initial_pos[i,1]:sq2fill[i,1]]<-diagram[initial_pos[i,2],initial_pos[i,1]:sq2fill[i,1]]+1
    
  }else if (sq2fill[i,1]!=0 & sq2fill[i,2]!=0) {
    diagram[initial_pos[i,2]+1:sq2fill[i,2],initial_pos[i,1]+1:sq2fill[i,1]]<-diagram[initial_pos[i,2]:sq2fill[i,2],initial_pos[i,1]:sq2fill[i,1]]+1
  
  }
}

