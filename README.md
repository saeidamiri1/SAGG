# SAGG
Supplementary codes for "How Aggregating improves the Classification's accuracy".  
This page explains the numerical evaluations done on the real data in Section 5.  

# I. Install the 'sagg' library.
The code were done under R version 4.2.1.

```
library("devtools")
devtools::install_github('saeidamiri1/sagg')
```

# II.  Load libraries
##### load library
```
library(sagging)
```

##### Load the following libraries in your R
```
library(tree)
library(class)
library(MASS)
library(adabag)
library("randomForest")
library("xgboost")

```

# III. Read Data
##### Load Sonar data to achieve the numerical analysis
```
data("Sonar")
Class<-NULL
Class[Sonar[,ncol(Sonar)]=="R"]<-1
Class[Sonar[,ncol(Sonar)]=="M"]<-2
Sonar<-cbind(Sonar[,-ncol(Sonar)],Class)
Dat<-Sonar
```

# IV. Run the simulations
```
B<-200
RF_res<-boost_res<-xgboost_res<-tc_res<- knn_res<-  bagg_tc_res<- sagg_tc_res <- bagg_knn_res <-sagg_knn_res<- NULL
i<-1

while (i<1000){
  N<-dim(Dat)[1]
  T0<-N*.2


  T<-sample(N,T0,replace=FALSE)
  L<-setdiff(c(1:N),T)

  DatL0<-Dat[L,-ncol(Dat)]
  DatT0<-Dat[T,-ncol(Dat)]
  ClassL0<-Dat[L,ncol(Dat)]
  ClassT0<-Dat[T,ncol(Dat)]


RF_m<-RF(DatL=DatL0,DatT=DatT0,ClassL=ClassL0)
RF_res[i]<-mean(abs(ClassT0-as.numeric(RF_m)!=0))

boost_m<-boostR(DatL=DatL0,DatT=DatT0,ClassL=ClassL0)
boost_res[i]<-mean(abs(ClassT0-as.numeric(boost_m)!=0))

xgboost_m<-xgboostR(DatL=DatL0,DatT=DatT0,ClassL=ClassL0,K0=2)
xgboost_res[i]<-mean(abs(ClassT0-as.numeric(xgboost_m)!=0))


tc_m<-tree.class(DatL=DatL0,DatT=DatT0,ClassL=ClassL0)
tc_res[i]<-mean(abs(ClassT0-tc_m)!=0)

knn_m<-KNN.class(DatL=DatL0,DatT=DatT0,ClassL=ClassL0)
knn_res[i]<-mean(abs(ClassT0-as.numeric(knn_m))!=0)

bagg_tc<-bagg(DatL=DatL0,DatT=DatT0,ClassL=ClassL0,B,method="TC")
bagg_tc_res[i]<-mean(abs(ClassT0-bagg_tc)!=0)

sagg_tc<-sagg(DatL=DatL0,DatT=DatT0,ClassL=ClassL0,B,Nsub=10,alpha=.8,method="TC")
sagg_tc_res[i]<-mean(abs(ClassT0-sagg_tc)!=0)

bagg_knn<-bagg(DatL=DatL0,DatT=DatT0,ClassL=ClassL0,B,method="KNN")
bagg_knn_res[i]<-mean(abs(ClassT0-bagg_knn)!=0)
  
sagg_knn<-sagg(DatL=DatL0,DatT=DatT0,ClassL=ClassL0,B,Nsub=10,alpha=.8,method="KNN")
sagg_knn_res[i]<-mean(abs(ClassT0-sagg_knn)!=0)

  i<-i+1
}
```

```


round(c(mean(RF_res),mean(boost_res),mean(xgboost_res),mean(tc_res),mean(knn_res),mean(bagg_tc_res),mean(sagg_tc_res), mean(bagg_knn_res),mean(sagg_knn_res)),3)

round(c(sd(RF_res),sd(boost_res),sd(xgboost_res),sd(tc_res),sd(knn_res),sd(bagg_tc_res),sd(sagg_tc_res), sd(bagg_knn_res),sd(sagg_knn_res)),3)
```