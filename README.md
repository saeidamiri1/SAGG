# SAGG
Supplementary codes for "Amiri, S. (2020). Classification via Aggregating on permuted Variables".  

Note: This is an early test release of the package, and only for experimental use. 
******* If you see any errors, please send an email to "saeid.amiri1@gmail.com" with subject "sagg" *******

This page explains the numerical evaluations done on the real data in Section 5.2.  


#I. Installing the 'sagging' library.
########################The coding are done under R version 3.3.1.
########################Github: https://github.com/saeidamiri1/sagg
####################################################################################
####################################################################################
####################################################################################
########################################## Install lktuple library in your R\Rstudio
##### Under Mac: the source is  stored in the Github, to install the sagging library in R, just run the following command
install.packages("https://github.com/saeidamiri1/sagg/blob/master/sagging_0.5.tgz?raw=true", repos = NULL, type="source")

##### Under Windows: the source is  stored in the Github, to install the sagging library in R, 
####  first download it from [link]
####  run the following command to install library  

install.packages("downloadfolder\sagging_0.5.zip", repos=NULL, type="source")


#II.  Load libraries
##################################
#### load library
library(sagging)

##################################
#### Load the following libraries in your R
library(tree)
library(class)
library(ESKNN)


#III. Read Data
##################################
#### Load Sonar data to achieve the numerical analysis
data("Sonar")
Class<-NULL
Class[Sonar[,ncol(Sonar)]=="R"]<-1
Class[Sonar[,ncol(Sonar)]=="M"]<-2
Sonar<-cbind(Sonar[,-ncol(Sonar)],Class)
Dat<-Sonar

#IV. Run the simulations

B<-150
seb1<-seb2<-seb3<-seb4<-seb5<-seb6<-seb7<-seb8<-seb9<-NULL
i<-1

while (i<1000){
  N<-dim(Dat)[1]
  T0<-N*.2
  L0<-N-T0

  T<-sample(N,T0,replace=FALSE)
  L<-setdiff(c(1:N),T)

  DatL0<-Dat[L,-ncol(Dat)]
  DatT0<-Dat[T,-ncol(Dat)]
  ClassL0<-Dat[L,ncol(Dat)]
  ClassT0<-Dat[T,ncol(Dat)]

  s1<-tree.class(DatL=DatL0,DatT=DatT0,ClassL=ClassL0)
  seb1[i]<-mean(abs(ClassT0-s1)!=0)
  s1<-bagg(DatL=DatL0,DatT=DatT0,ClassL=ClassL0,B,method="TC")
  seb2[i]<-mean(abs(ClassT0-s1)!=0)
  s1<-sagg(DatL=DatL0,DatT=DatT0,ClassL=ClassL0,B,Nsub=10,alpha=.8,method="TC")
  seb3[i]<-mean(abs(ClassT0-s1)!=0)
  s1<-sagg(DatL=DatL0,DatT=DatT0,ClassL=ClassL0,B,Nsub=10,alpha=1,method="TC")
  seb4[i]<-mean(abs(ClassT0-s1)!=0)

  model<-esknnClass(DatL0, ClassL0,k=NULL)
  resClass<-Predict.esknnClass(model,DatT0,ClassT0,k=NULL)
  seb5[i]<-mean(abs(ClassT0-resClass$PredClass)!=0)

  s1<-KNN.class(DatL=DatL0,DatT=DatT0,ClassL=ClassL0)
  seb6[i]<-mean(abs(ClassT0-s1)!=0)
  s1<-bagg(DatL=DatL0,DatT=DatT0,ClassL=ClassL0,B,method="KNN")
  seb7[i]<-mean(abs(ClassT0-s1)!=0)
  
  s1<-sagg(DatL=DatL0,DatT=DatT0,ClassL=ClassL0,B,Nsub=10,alpha=.8,method="KNN")
  seb8[i]<-mean(abs(ClassT0-s1)!=0)
  s1<-sagg(DatL=DatL0,DatT=DatT0,ClassL=ClassL0,B,Nsub=10,alpha=1,method="KNN")
  seb9[i]<-mean(abs(ClassT0-s1)!=0)

  i<-i+1
}


> round(c(mean(seb1),mean(seb2),mean(seb3),mean(seb4),mean(seb5),mean(seb6),mean(seb7), mean(seb8), mean(seb9)),3)

[1] 0.288 0.212 0.158 0.157 0.393 0.182 0.497 0.129 0.132

>  round(c(sd(seb1),sd(seb2),sd(seb3),sd(seb4),sd(seb5),sd(seb6),sd(seb7), sd(seb8), sd(seb9)),4)

[1] 0.0650 0.0662 0.0580 0.0575 0.0852 0.0525 0.0752 0.0530 0.0539
