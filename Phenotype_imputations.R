setwd("/impute")

library(readxl)
library(NAM)
library(plyr)
library(dplyr)
library(tidyverse)



####cluster1
Geno<- read.csv('combined_progenygenotypeCL1.csv') ##this file was obtaining using python script "coca_csv_files.py"
#Geno[1:10,1:10]

##subsetting Parents
Parents<- Geno[grep("^PID",Geno$LINE),]
Parents<- Parents %>% distinct(LINE, .keep_all = TRUE)

Parents<- Parents[,-c(1,2, 2915)]
rownames(Parents)<-Parents[,1]
Parents<-Parents[,-1]


##Subssetting the genotype information for progeny
Progeny<- Geno[-(grep("^PID",Geno$LINE)),]
Progeny<- Progeny[-which(Progeny$LINE == ""), ]

##leaving the one column with the file name as C1.##
filename<- Progeny$filename
filename<-gsub(".combinedgeno.csv","", x=filename, perl = TRUE)
##deleting last column and replacing but the new names
Progeny<-Progeny[,-c(1,2,2915)]
##joiniing filename and data
Progeny<- cbind(filename,Progeny)

Progeny$LINE<- as.integer(as.character(Progeny$LINE))
range(Progeny$LINE, na.rm = TRUE)

###cluster 1 Pheno
Pheno<- read.csv('/combined_progenyPhenotypeCL1.csv') ##this file was obtaining using python script "coca_csv_files.py"
max(Pheno$STLP, na.rm = TRUE)
Pheno[420564,13]<- NA
#range(Pheno$LINE)

filename<- Pheno$filename
filename<-gsub(".progenyPhenotype.csv","", x=filename, perl = TRUE)
##deleting last column and replacing but the new names
Pheno<-Pheno[,-19]
##joiniing filename and data
Pheno<- cbind(filename,Pheno)
Pheno$LINE<- as.factor(as.character(Pheno$LINE))
Pheno$filename<- as.factor(as.character(Pheno$filename))


##merging geno and file of cluster 1
Pheno<- as.tibble(Pheno)
Pheno$LINEID<- with(Pheno, paste0(filename,".", LINE))
Pheno<- Pheno[,c(20,1:19)]
length(unique(Pheno$LINEID))


Progeny$filename<- as.factor(as.character(Progeny$filename))
Progeny$LINE<- as.factor(as.character(Progeny$LINE))
Progeny$LINEID<- with(Progeny, paste0(filename,".", LINE))
Progeny<- Progeny[,c(1,2,2914,3:2913)]
Progeny<- Progeny[!duplicated(Progeny$LINEID),] ##delete the rows with missing line ID
rownames(Progeny)<- Progeny[,3]
Progeny<-Progeny[,-c(1,2,3)]


#df1<-Progeny[duplicated(Progeny$LINEID),]

##checking data imputation
##joinging parents and progeny
gen<- rbind(Parents1, Progeny1)

##impute by chromosome
##loading marker info
Markers_infoC1 <- read_csv("/class/datamine/corporate/bayer/Diana_Escamilla/Descriptive_statistics/Markers_infoC1.csv")
Markers_infoC1<- Markers_infoC1[,-1]

####
library(missForest)
##for this fucntion you need to provide the the genotype info (x) and the marker information (M)
Func1<- function(x,M){
  impX<- c()
  chr<- seq(1:10)
  for (i in chr) {
    a<-M[M$Chromosome == i,]
    list<- a$markers
    x<- x[,(names(x)%in% list)]
    ximp<- missForest::missForest(x)
    impX<- cbind(impX, ximp$Ximp)
  }
  return(impX)
  
}


impX<- Func1(gen,Markers_infoC1)

write.csv(impX, file="impXCL1.Rdata")








