# FILENAME: server1.R
#######loading libraries###
#install.packages('readr',repos = "http://ftp.ussg.iu.edu/CRAN/")
#library(readr)
#install.packages('plyr',repos = "http://ftp.ussg.iu.edu/CRAN/")
#library(plyr)
#install.packages('dplyr',repos = "http://ftp.ussg.iu.edu/CRAN/")
#library(dplyr)
#install.packages('tidyverse',repos = "http://ftp.ussg.iu.edu/CRAN/")
#library(tidyverse)
#install.packages('parallel',repos = "http://ftp.ussg.iu.edu/CRAN/")
#library(parallel)
install.packages('data.table',repos = "http://ftp.ussg.iu.edu/CRAN/")
library(data.table)
#install.packages('randomForest',repos = "http://ftp.ussg.iu.edu/CRAN/")
#library(randomForest)
#install.packages('caret',repos = "http://ftp.ussg.iu.edu/CRAN/")
#library(caret)
#install.packages('doParallel',repos = "http://ftp.ussg.iu.edu/CRAN/")
#library(doParallel)
#install.packages('doMC',repos = "http://ftp.ussg.iu.edu/CRAN/")
#library(doMC)
install.packages('h2o',repos = "http://ftp.ussg.iu.edu/CRAN/")
#library(mlbench)
library(h2o)

####removing all files and loading the final file
CL1_PXGXE<-fread("CL1_PxGxE_allvariables1.csv", showProgress = FALSE)
#CL1_PXGXE<-fread("CL1_PXGXE.csv", showProgress = FALSE)
CL1_PXGXE<-CL1_PXGXE[,-1]
#CL1_PXGXE<-CL1_PXGXE[1:1000,]
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
#rm(Hold_dat)
CL1_PXGXE<- CL1_PXGXE[which(CL1_PXGXE$YEAR!=years[9]),]

# Using random forest for variable selection and prediction modles
#rm(CL1_PXGXE)
#dat1<-as.tibble(dat1)
CL1_PXGXE<- CL1_PXGXE[, c(1,4,15,22:3001)]
CL1_PXGXE<- CL1_PXGXE[,-c(71,72)]
#CL1_PXGXE<- as.numeric(CL1_PXGXE)

###setting up the cross validation
years<-years[-9]

##setting up the cross validation withing 
year<- 2001
##split dataset in training and test
test<-CL1_PXGXE[which(CL1_PXGXE$YEAR== year),]
test<- test[,-c(1,2)]
test[, names(test) := lapply(.SD, as.numeric)]
test<- na.omit(test)
#train<-as.matrix(train)
#test1<- test[,-1]
#test1<- scale(test1)
#centering the predictors
#test<- cbind(test[,1], test1)
#rm(test1)
#test<- test[,c(1,69:2979)]

train<-CL1_PXGXE[which(CL1_PXGXE$YEAR != year),]
rm(CL1_PXGXE)
##train the model runing Random forest algorithm with 5 times repeated 10 fold CV
train<- train[,-c(1,2)]
train[, names(train) := lapply(.SD, as.numeric)]
train<- na.omit(train)
#train<-as.matrix(train)
#train1<- train[,-1]
#train1<- scale(train1)
##centering the predictors
#train<- cbind(train[,1], train1)
#rm(train1)
#train<- train[,c(1,69:2979)]
###fititng the  

#registerDoMC(cores = 100) # convention to leave 1 core for OS
#registerDoParallel(40)


#rf.fit <- train( x= train[,-1], 
#                 y=train$YLD_BE, 
#                 method = "rf",     # Use the "random forest" algorithm
#                 importance = TRUE,
#                 # importance=TRUE allows to inspect variable importance
#                 trControl = trainControl(method = "repeatedcv", # Use cross-validation
#                                          number = 10, # Use 10 folds for cross-validation
#                                          repeats = 2, allowParallel = TRUE))


##initializing h20 cluster
h2o.init(nthreads = -1)

##now we transfer the data to h2o cluster 
train.h2o <- as.h2o(train)
#train.h2o<-h2o.scale(train.h2o[2:2979])

train.split<- h2o.splitFrame(data=train.h2o,ratios = 0.75)

train.h2o<- train.split[[1]]
val.h2o<- train.split[[2]]

test.h2o <- as.h2o(test)
#test.h2o<-h2o.scale(test.h2o[2:2979])


y.dep<- "YLD_BE"
x.indep<- setdiff(names(test), y.dep)

# Build and train the model:
#rf <- h2o.randomForest(x = x.indep,
#                             y = y.dep,
#                             ntrees = 200,
#                             max_depth = 5,
#                             nfolds = 10,
#                             training_frame = train.h2o,)



# Set random grid search criteria: 
search_criteria <- list(strategy = "RandomDiscrete",
                         stopping_metric = "deviance",
                         stopping_tolerance = 0.005,
                         stopping_rounds = 10,
                         max_runtime_secs = 60*60)


# Set hyperparameter grid: 

hyper_grid.h2o <- list(ntrees = seq(50, 300, by = 50),
                       mtries = seq(3, 20, by = 1),
                       max_depth = seq(10, 30, by = 10),
                       # min_rows = seq(1, 3, by = 1),
                       # nbins = seq(20, 30, by = 10),
                       sample_rate = c(0.55, 0.632, 0.75))                     

# Turn parameters for RF: 
random_grid <- h2o.grid(algorithm = "randomForest",
                        grid_id = "rf_grid2",
                        x = x.indep, 
                        y = y.dep, 
                        seed = 29, 
                        nfolds = 10, 
                        training_frame = train.h2o,
                        validation_frame = val.h2o,
                        hyper_params = hyper_grid.h2o,
                        search_criteria = search_criteria)



save(random_grid,file="rf.fit.RData")

grid_perf2 <- h2o.getGrid(grid_id = "rf_grid2", 
                          sort_by = "residual_deviance", 
                          decreasing = FALSE)

print(grid_perf2)

#summary(random_grid)

# Best RF: 
best_model1 <- h2o.getModel(grid_perf2@model_ids[[1]])
print(best_model1)


model_path<- h2o.saveModel(object = best_model1,path = getwd(),force = TRUE)
print(model_path)

#load model
saved_model <- h2o.loadModel(model_path)
# download the model built above to your local machine
my_local_model1 <- h2o.download_model(best_model1, path = "/class/datamine/corporate/bayer/students/Diana_Escamilla/impute")


# Use this best model for prediction


rf.predict <- h2o.predict(best_model1, test.h2o)

summary(rf.predict)


print(rf.predict)
###########
#rf.predict<-predict(rf.fit , test)
save(rf.predict,file="rf.predict1.RData")

#stopCluster(cluster)
#library(h2o)
#h2o.init()
model_path <- "/class/datamine/corporate/bayer/students/Diana_Escamilla/impute/rf_grid2_model_1"
saved_model <- h2o.loadModel(model_path)

rf.predict <- h2o.predict(saved_model, test.h2o)
a<-as.data.frame(rf.predict)
a<- a$predict
b<- test$YLD_BE

cor(a,b)


h2o.performance(model = saved_model,
                newdata = test.h2o)

summary(saved_model)


h2o.shutdown()
