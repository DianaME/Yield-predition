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
#nstall.packages('data.table',repos = "http://ftp.ussg.iu.edu/CRAN/")
library(data.table)
install.packages('adegenet',repos = "http://ftp.ussg.iu.edu/CRAN/")
library(adegenet)
install.packages('NAM',repos = "http://ftp.ussg.iu.edu/CRAN/")
library(NAM)#
install.packages('ade4',repos = "http://ftp.ussg.iu.edu/CRAN/")
library(ade4)
#install.packages('mlr',repos = "http://ftp.ussg.iu.edu/CRAN/")
#library(mlr)
#library(doMC)
#install.packages('h2o',repos = "http://ftp.ussg.iu.edu/CRAN/")
#library(mlbench)
#library(h2o)


##Genotype from probability imputation
library(adegenet)
library(ade4)
library(NAM)
library(stringr)
Geno<- read.csv('/class/datamine/corporate/bayer/students/Diana_Escamilla/impute/impGenoCL1_prob.csv') ##ths is the geno file using genotype imputation by probabilities

Geno<- Geno[,-1]
grp1<- Geno$filename
Geno$Line<- str_remove(Geno$Line,"^0+")
#Geno<- Geno[-which(Geno$Line == ""), ]

Geno$filename<-gsub("c1.","", x=Geno$filename, perl = TRUE)
Geno$LINEID<- with(Geno, paste0(filename,"_", Line))
Geno<- Geno[,c(2914,1:2913)]
Geno<- Geno[,-c(2,3)]


####PCA ###
rownames(Geno)<- Geno$LINEID
Geno<- Geno[,-1]
Geno <-lapply(Geno,as.numeric)
Geno<- as.data.frame(Geno)
#Geno<- IMP(Geno)
Geno<- snpQC(Geno, MAF=0.05, impute = TRUE)

#grp<- find.clusters(Geno, max.n.clust = 100) ###i save 225 PCs
#save(grp, file="grp.RData" )

#names(grp)
#head(grp$Kstat, 50)
#grp$stat
#grp$size


dapc1 <- dapc(Geno, grp1, n.pca=225)

save(dapc1, file = "dapc1_fam.RData")

scatter(dapc1)
scatter(dapc1, posi.da="bottomright", bg="white", pch=17:22)

color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]


myCol <-unique(sample(color, 499, replace = TRUE))
pie(rep(1,499), col=myCol)

scatter(dapc1, posi.da="bottomright", bg="white",
        pch=17:22, cstar=0, col=myCol, scree.pca=TRUE,
        posi.pca="bottomleft")

scatter(dapc1, scree.da=FALSE, bg="white", pch=20, cell=0, cstar=0, col=myCol, solid=.4,
        cex=3,clab=0, leg=TRUE, txt.leg=paste("Cluster",1:30))


scatter(dapc1, ratio.pca=0.3, bg="white", pch=20, cell=0,
        cstar=0, col=myCol, solid=.4, cex=3, clab=0,
        mstree=TRUE, scree.da=FALSE, posi.pca="bottomright",
        leg=TRUE, txt.leg=paste("Cluster",1:30))
par(xpd=TRUE)
points(dapc1$grp.coord[,1], dapc1$grp.coord[,2], pch=4,
       cex=3, lwd=8, col="black")
points(dapc1$grp.coord[,1], dapc1$grp.coord[,2], pch=4,
       cex=3, lwd=2, col=myCol)

myInset <- function(){
  temp <- dapc1$pca.eig
  temp <- 100* cumsum(temp)/sum(temp)
  plot(temp, col=rep(c("black","lightgrey"),
                     c(dapc1$n.pca,2911)), ylim=c(0,100),
       xlab="PCA axis", ylab="Cumulated variance (%)",
       cex=1, pch=20, type="h", lwd=2)
}
add.scatter(myInset(), posi="bottomleft",
            inset=c(-0.03,-0.01), ratio=.28,
            bg=transp("white"))

contrib <- loadingplot(dapc1$var.contr, axis=2,
                       thres=.0082, lab.jitter=1)

contrib<- dapc1$var.contr

scatter(dapc1,2,2, col=myCol, bg="white",
        scree.da=FALSE, legend=TRUE, solid=.4)

##LDA 1 M00003695925 M00000208905 M00009449812

M1<- Geno[, "M00009449812"]

summary(M1)
hist(M1)


####dapc posterior 
library(mltools)
library(data.table)
library(caret)
posterior<-dapc1$assign


dat<- as.data.frame(posterior)
dat$posterior<- as.character(dat$posterior)
colnames(dat)<-"GenCl"

dummy <- dummyVars(" ~ .", data=dat)
newdata <- data.frame(predict(dummy, newdata = dat)) 


##loading again genotype

Geno<- read.csv('/class/datamine/corporate/bayer/students/Diana_Escamilla/impute/impGenoCL1_prob.csv') ##ths is the geno file using genotype imputation by probabilities

Geno<- Geno[,-1]

Geno$Line<- str_remove(Geno$Line,"^0+")
#Geno<- Geno[-which(Geno$LINE == ""), ]
Geno$LINEID<- with(Geno, paste0(filename,".", Line))
Geno<- Geno[,c(2914,1:2913)]
Geno<- Geno[,-c(2,3)]


newdata$LINEID<- Geno$LINEID
newdata<- newdata[,c(500,1:499)]
write.csv(newdata,file = "DAPC_CL1.csv") ##saving a dataframe with the resutls of the discrimiinant analysis

ENV<- read.csv("final_impENVCL1.cvs")
ENV<- ENV[,-1]

##joiniing filename and data
prog_phen_env_ALL <- read.csv("prog_phen_env_ALL.csv")##this is sven file
#rm(prog_phen_env_ALL)

datc1<-  prog_phen_env_ALL[which(prog_phen_env_ALL$CLUSTER== 1),]
datc2<- prog_phen_env_ALL[which(prog_phen_env_ALL$CLUSTER== 2),]


#ENV2<-datc2[,20:206]
#b<- seq(1:187)
#ENV2[,b]<- lapply(ENV2[,b] , as.numeric)
#ENV2<- as.matrix(ENV2)


impX<-cbind(datc1[,c(3,5,6,7:15,18 )], ENV)
##creating the identifier in environmental data 
impX$LINE<- str_remove(impX$LINE,"^0+")
folder<-gsub("C1_","", x=impX$folder_id, perl = TRUE)
folder<- str_remove(folder,"^0+")

impX<-impX %>%
  mutate(LINEID = paste0("c1.",folder, ".", LINE))

impX<- impX[,c(157,1:156)]
impX<-impX[,-c(5,14)]

dat<- merge(impX, newdata, by="LINEID", all.x = TRUE) ##pheno and geno
write.csv(dat, file="DAPC_GXEXP_Cl1.csv") ## writing a file with discriminat analysis information and environment and phenotype data


