library(tidyverse)

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

for (j in 1:sheets){
  add<-c(0,5,10,15,20)
  add=add+25*(j-1)
  for (i in 1:5){
    add=add+1
    array[,i,j]<-bingo_numM[add]
  }
}

array_hits<-array(0,dim = c(5,5,sheets))
complete_r<-array(0,dim = c(5,5,sheets))
complete_c<-array(0,dim = c(5,5,sheets))
cr<-array(0,dim = c(5,1,sheets))
cc<-array(0,dim = c(1,5,sheets))
stop=0
for(h in 1:length(num)){
  hits<-seq(length(array))[array %in% num[h]]
  array_hits[hits]<-1
  for (j in 1:sheets){
    for (i in 1:5){
      complete_r[i,,j]<-array_hits[i,,j]==c(1,1,1,1,1)
      cr[i,1,j]<-sum(complete_r[i,,j])
      complete_c[,i,j]<-array_hits[,i,j]==c(1,1,1,1,1)
      cc[1,i,j]<-sum(complete_c[,i,j])
      
      if (cr[i,1,j]==5||cc[1,i,j]==5){
        print("WINNER")
        print(num[h])
        print("Board")
        print(j)
        nothit<-which(array_hits[,,j]!=1)
        sum<-sum(array[,,j][nothit])
        print(score<-sum*num[h])
        stop=1 
        break
      }
    }
    if (stop==1){
      break
    }
  }
  if (stop==1){
    break
  }
}

#day 4 part 2

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

for (j in 1:sheets){
  add<-c(0,5,10,15,20)
  add=add+25*(j-1)
  for (i in 1:5){
    add=add+1
    array[,i,j]<-bingo_numM[add]
  }
}

array_hits<-array(0,dim = c(5,5,sheets))
complete_r<-array(0,dim = c(5,5,sheets))
complete_c<-array(0,dim = c(5,5,sheets))
cr<-array(0,dim = c(5,1,sheets))
cc<-array(0,dim = c(1,5,sheets))
stop=0
winning_boards<-array(0,dim=c(sheets,1))
for(h in 1:length(num)){
  hits<-seq(length(array))[array %in% num[h]]
  array_hits[hits]<-1
  for (j in 1:sheets){
    for (i in 1:5){
      complete_r[i,,j]<-array_hits[i,,j]==c(1,1,1,1,1)
      cr[i,1,j]<-sum(complete_r[i,,j])
      complete_c[,i,j]<-array_hits[,i,j]==c(1,1,1,1,1)
      cc[1,i,j]<-sum(complete_c[,i,j])
      
      if (cr[i,1,j]==5||cc[1,i,j]==5){
        print("WINNER")
        print(num[h])
        print("Board")
        print(j)
        winning_boards[j]<-1
        if (sum(winning_boards)==100){
        nothit<-which(array_hits[,,j]!=1)
        sum<-sum(array[,,j][nothit])
        score<-sum*num[h]
        string<-"Score="
        print(paste(string,score))
        stop=1 
        break
          }
      }
      if (stop==1){
        break 
    }
    }
    if (stop==1){
      break
    }
  }
  if (stop==1){
    break
  }
  }