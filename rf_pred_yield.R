# FILENAME: server1.R
#######loading libraries###
install.packages('readr',repos = "http://ftp.ussg.iu.edu/CRAN/")
library(readr)
install.packages('plyr',repos = "http://ftp.ussg.iu.edu/CRAN/")
library(plyr)
install.packages('dplyr',repos = "http://ftp.ussg.iu.edu/CRAN/")
library(dplyr)
install.packages('tidyverse',repos = "http://ftp.ussg.iu.edu/CRAN/")
library(tidyverse)
install.packages('parallel',repos = "http://ftp.ussg.iu.edu/CRAN/")
library(parallel)
install.packages('data.table',repos = "http://ftp.ussg.iu.edu/CRAN/")
library(data.table)
install.packages('randomForest',repos = "http://ftp.ussg.iu.edu/CRAN/")
library(randomForest)

####removing all files and loading the final file
CL1_PXGXE<-fread("CL1_PXGXE.csv", showProgress = FALSE)

CL1_PXGXE<-CL1_PXGXE[,-1]
###checking the amount of years
#unique(CL1_PXGXE$YEAR)


#dat<- dat[1:100, c(18,39:2949)]
#dat<- na.omit(dat)

####holding last year as a hold out dataset 
##checking for the list of years
years<-unique(CL1_PXGXE$YEAR)
years<- sort(years,decreasing = FALSE)

#Hold_dat<- CL1_PXGXE[which(CL1_PXGXE$YEAR==years[9]),]
#write.csv(Hold_dat,file="hold.csv")
CL1_PXGXE<- CL1_PXGXE[which(CL1_PXGXE$YEAR!=years[9]),]

# Using random forest for variable selection and prediction modles
#rm(CL1_PXGXE)
#dat1<-as.tibble(dat1)
CL1_PXGXE<- CL1_PXGXE[, c(7,18,22:2949)]
CL1_PXGXE<- CL1_PXGXE[,-c(18,19)]
#CL1_PXGXE<- as.numeric(CL1_PXGXE)

###setting up the cross validation
years<-years[-9]

rf<-function(year){
  test<-CL1_PXGXE[which(CL1_PXGXE$YEAR== year),]
  train<-CL1_PXGXE[which(CL1_PXGXE$YEAR != year),]
  train<- na.omit(train)
  train<-train[,-1]
  train[, names(train) := lapply(.SD, as.numeric)]
  train<-as.matrix(train)
  rf <- randomForest(y=train[,1],x=train[,-1], data = train, ntree=200,  norm.votes=FALSE, max_depth = 6, importance=TRUE)
  #Evaluate variable importance
  impor<-as.data.frame(importance(rf))
  average<- mean(impor[,1])
  # select those features which importance is greater than the mean importance of all the features by default, but we can alter this threshold if we want.
  impor<- impor[which(impor$`%IncMSE` >= average),]
  name<- paste0("impor", year, "out",".csv")
  write.csv(impor, file = name)
  ##converting training and test set in the appropriate
  rown<- rownames(impor)
  train.1<- train[,colnames(train) %in% (rown)] 
  train.1<-cbind(train[,1],train.1)
  ##test
  test<-as.matrix(test)
  test.1<- test[,colnames(test)%in%(rown)]
  test.1<- cbind(test[,2], test.1)
  test.1<- na.omit(test.1)
  rf.1 <- randomForest(y=train.1[,1],x=train.1[,-1], data = train.1, ntree=200,  max_depth = 6)
  predictForest <- predict(rf.1, newdata = test.1[,-1])
  result<- tes.1[,-1]
  result['yield']<-test.1[,1]
  result['prediction']<- predictForest
  name1<- paste0("predic", year, "out",".csv")
  write.csv(result, file = name1)
}

results<- mclapply(years, rf, mc.cores = 20)

save(results, file="rf_CL1.RData")



