library(tidyverse)
library(ggplot2)

#day 1 - part 1
input=scan(file="C:\\Users\\Michael\\OneDrive - University of Calgary\\Advent_of_code\\2021\\AOC1.txt")
input_short=input[2:length(input)]
zero=c(0)
input_short=c(input_short,zero)
input[length(input)]<-0;input

input.df=data.frame(input)
input.df$input_shift<-input_short
diff=input.df$input_shift-input.df$input
length(diff[diff>0])

#input[length(input)]#get last value of vector

#day 1 - part 2
rm(list=ls())#clear workspace 
input=scan(file="C:\\Users\\Michael\\OneDrive - University of Calgary\\Advent_of_code\\2021\\AOC1.txt")

summed<-numeric()
for (i in 1:(length(input)-2)){
  sum=sum(input[i],input[i+1],input[i+2])
  summed<-c(summed,sum)
}
summed_short=summed[2:length(summed)]
zero=c(0)
summed_short=c(summed_short,zero)
summed[length(summed)]<-0;summed
diff=summed_short-summed
length(diff[diff>0])

#day 2 - part 1
rm(list=ls())#clear workspace 
data<-read.table(file="C:\\Users\\Michael\\OneDrive - University of Calgary\\Advent_of_code\\2021\\AOC2.txt")#creates data frame with two colums for direction and magnitude of change
x="forward"
ydown="down"
yup="up"

move_x<-numeric()
move_down<-numeric()
move_up<-numeric()
for (i in 1:length(data$V1)){
  
  move_x<-which(data$V1==x)
  move_down<-which(data$V1==ydown)
  move_up<-which(data$V1==yup)
}
sum_x=sum(data$V2[move_x])
sum_down=sum(data$V2[move_down])
sum_up=sum(data$V2[move_up])
move_x=sum_x
move_y=sum_down-sum_up
product=move_x*move_y
product

#day 2 - part 2
rm(list=ls())#clear workspace 
data<-read.table(file="C:\\Users\\Michael\\OneDrive - University of Calgary\\Advent_of_code\\2021\\AOC2.txt")#creates data frame with two colums for direction and magnitude of change


move_down<-numeric()

aim<-c(0)
depth<-c(0)
horz<-c(0)
for (i in 1:length(data$V1)){
  if(data$V1[i]=="forward"){
    horz=horz+data$V2[i]
    move_down=aim*data$V2[i]
    depth=depth+move_down
  } else if (data$V1[i]=="down"){
    aim=aim+data$V2[i]
  }else {
    aim=aim-data$V2[i]
  }
}
product=horz*depth
product

#day 3 - part 1
rm(list=ls())#clear workspace 
d <- read.csv("C:\\Users\\Michael\\OneDrive - University of Calgary\\Advent_of_code\\2021\\AOCT3.csv", fileEncoding="UTF-8-BOM",header=F,colClasses=c("character"))#numeric data cannot have leading or trailing zeros
length=length(d$V1)
value<-matrix(ncol=12,nrow=length)
for (i in 1:length(d$V1)){
  value[,i]=substr(d$V1,i,i)
  
}
value.df=data.frame(value)
mode=apply(value.df,2,function(x) names(which.max(table(x))))
gamma=paste(mode,collapse="")
mode=apply(value.df,2,function(x) names(which.min(table(x))))
epsilon=paste(mode,collapse="")
gamma_int=strtoi(gamma,base=2)
epsilon_int=strtoi(epsilon,base=2)
gamma_int*epsilon_int

#day 3 - part 2
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

#Day 4 - part 1
rm(list=ls())#clear workspace 
d <- read.csv("C:\\Users\\Michael\\OneDrive - University of Calgary\\Advent_of_code\\2021\\AOCT4.csv", fileEncoding="UTF-8-BOM",header=F,colClasses=c("character"))#numeric data cannot have leading or trailing zeros
val<-separate_rows(d,V1, convert = TRUE)

num<-val$V1[1:27]
bc<-data.frame(d$V1[2:length(d$V1)])
colnames(bc)<-c('V1')

str_length=nchar(d[1,])
str_length=str_length[1]
value<-matrix(ncol=str_length,nrow=length)

for (i in 1:str_length){
  value[,i]=as.numeric(substr(d[1,],i,i))
}
value.df=data.frame(value)

d$V1[1]

for(i in 1:nrow(bc)){
  for(j in 1:20){
    as.numeric(substr(bc$V1[i],j,j))
  }
}

#day 4 P1 try 2
rm(list=ls())#clear workspace 
input=scan(file="C:\\Users\\Michael\\OneDrive - University of Calgary\\Advent_of_code\\2021\\AOC4.txt",what='numeric')
input<-data.frame(input)
val<-separate_rows(input,input, convert = TRUE)
num_l<-length(val$input)-(length(input$input)-1)
num<-val$input[1:num_l]
bingo_numM<-val$input[(num_l+1):length(val$input)]

#number of sheets needed
sheets<-length(bingo_numM)/25
array<-array(0,dim = c(5,5,sheets))
col<-sheets*5
add<-c(0,5,10,15,20)
for (j in 1:sheets){
  for (i in 1:5){
    add=add+25*(j-1)
    add=add+1
    array[,i,j]<-bingo_numM[add]
  }
}

array_hits<-array(0,dim = c(5,5,sheets))
complete_r<-array(0,dim = c(5,5,sheets))
complete_c<-array(0,dim = c(5,5,sheets))
cr<-array(0,dim = c(5,5,sheets))
cc<-array(0,dim = c(5,5,sheets))

sh1<-matrix(ncol=5,nrow=5)
sh2<-matrix(ncol=5,nrow=5)
sh3<-matrix(ncol=5,nrow=5)

col<-sheets*5
add<-c(0,5,10,15,20)
add2<-c(25,30,35,40,45)
add3<-c(50,55,60,65,70)
for(i in 1:col){
  if (i<=5){
    add=add+1
    sh1[,i]<-bingo_numM[add]
  } else if (i>5 & i<=10){
    add2=add2+1
    sh2[, i-5]<-bingo_numM[add2]
  }else{
    add3=add3+1
    sh3[,i-10]<-bingo_numM[add3]
  }
}

sh1_hits<-matrix(ncol=5,nrow=5)
sh1_hits[,]<-0
sh2_hits<-matrix(ncol=5,nrow=5)
sh2_hits[,]<-0
sh3_hits<-matrix(ncol=5,nrow=5)
sh3_hits[,]<-0
complete_r<-matrix(ncol=5,nrow=5)
complete_c<-matrix(ncol=5,nrow=5)
cr<-matrix(ncol=1,nrow=5)
cc<-matrix(ncol=5,nrow=1)
complete_r2<-matrix(ncol=5,nrow=5)
complete_c2<-matrix(ncol=5,nrow=5)
cr2<-matrix(ncol=1,nrow=5)
cc2<-matrix(ncol=5,nrow=1)
complete_r3<-matrix(ncol=5,nrow=5)
complete_c3<-matrix(ncol=5,nrow=5)
cr3<-matrix(ncol=1,nrow=5)
cc3<-matrix(ncol=5,nrow=1)
#hits
for(j in 1:length(num)){
  hits1<-seq(length(sh1))[sh1 %in% num[j]]
  sh1_hits[hits1]<-1
  hits2<-seq(length(sh2))[sh2 %in% num[j]]
  sh2_hits[hits2]<-1
  hits3<-seq(length(sh3))[sh3 %in% num[j]]
  sh3_hits[hits3]<-1
  for (i in 1:5){
    complete_r[i,]<-sh1_hits[i,]==c(1,1,1,1,1)
    cr[i,]<-sum(complete_r[i,])
    complete_c[,i]<-sh1_hits[,i]==c(1,1,1,1,1)
    cc[,i]<-sum(complete_c[,i])
    
    complete_r2[i,]<-sh2_hits[i,]==c(1,1,1,1,1)
    cr2[i,]<-sum(complete_r2[i,])
    complete_c2[,i]<-sh2_hits[,i]==c(1,1,1,1,1)
    cc2[,i]<-sum(complete_c2[,i])
    
    complete_r3[i,]<-sh3_hits[i,]==c(1,1,1,1,1)
    cr3[i,]<-sum(complete_r3[i,])
    complete_c3[,i]<-sh3_hits[,i]==c(1,1,1,1,1)
    cc3[,i]<-sum(complete_c3[,i])
  }
  if (cr==5||cc==5){
    print("WINNER")
    print(num[j])
    print("Board 1")
    nothit<-which(sh1_hits!=1)
    sum<-sum(sh1[nothit])
    print(score<-sum*num[j])
    break
  }else if(cr2==5||cc2==5){
    print("WINNER")
    print(num[j])
    print("Board 2")
    nothit2<-which(sh2_hits!=1)
    sum<-sum(sh2[nothit2])
    print(score<-sum*num[j])
    break
  }else if (cr3==5||cc3==5) {
    print("WINNER")
    print(num[j])
    print("Board 3")
    nothit3<-which(sh3_hits!=1)
    sum<-sum(sh3[nothit3])
    print(score<-sum*num[j])
    break
  }
}


