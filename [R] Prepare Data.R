setwd("D:/Google Drive/BigData/Kaggle/[Case 4] Winton group")
#setwd("C:/Users/jsc69/Google Drive/BigData/Kaggle/[Case 3] Otto Group Product Classification Challenge")

train = read.csv("train.csv",header=TRUE,stringsAsFactors = F)
train_mat = matrix(unlist(train),40000,211)

#train_sample <- train
#smp_size <- floor(0.8 * nrow(train))
#set.seed(123)
#train_ind <- sample(seq_len(nrow(train)), size = smp_size)
# sample has the random ability
#mean(is.na(train_mat[,2]))  ## This is to check how mich (%) data lost.


## This calculate the sum of those valid numbers for each col
MeanOfCol<-c()
for (x in 1:ncol(train_mat)){
  MeanOfCol[x]<-mean(train_mat[,x], na.rm=TRUE)
}
  
## Make up those NA
train_mat_deal = train_mat
for (x in 1:ncol(train_mat)){
  for (y in 1:nrow(train_mat)){
    if (is.na(train_mat_deal[y,x])==TRUE){
      train_mat_deal[y,x]=MeanOfCol[x]
    }
  }
}

train_mat_intra <-train_mat_deal[,29:207]
train_mat_outtra <-train_mat_deal[,208:209]
#write.csv(train_sample , file = "Train_Trainsmall.csv", row.names = FALSE)
#write.csv(test_sample , file = "Train_Validsmall.csv", row.names = FALSE)

##now we have to normalize each two
energy <- c()
for (y in 1:nrow(train_mat_intra)){
  energy[y]=sqrt(sum(train_mat_intra[y,]*train_mat_intra[y,])/ncol(train_mat_intra))
}
train_mat_intra_norm = train_mat_intra
for (y in 1:nrow(train_mat_intra)){
  train_mat_intra_norm[y,] = train_mat_intra[y,]/energy[y]
}

intra_pre<-c()
for (x in 1:ncol(train_mat_intra)){
  intra_pre[x]=mean(train_mat_intra[,x])
}

outtra_pre<-c()
for (x in 1:ncol(train_mat_outtra)){
  outtra_pre[x]=mean(train_mat_outtra[,x])
}
