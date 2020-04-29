sagg<-function(DatL,DatT, ClassL, B,Nsub, alpha,method){
  if(method=="KNN") {
    Re<-saggKNN(DatL,DatT, ClassL, B,Nsub, alpha)
  }
  else{
    Re<-saggTC(DatL,DatT, ClassL, B,Nsub, alpha)
  }
  return(Re)
}

saggKNN<-function(DatL,DatT, ClassL, B,Nsub, alpha){

  DD<-scale(rbind(DatL,DatT))
  DatL<-scale(DatL, center = attr(DD,"scaled:center"), scale = attr(DD,"scaled:scale"))
  DatT<-scale(DatT, center = attr(DD,"scaled:center"), scale = attr(DD,"scaled:scale"))
  s0<-ClassL
  s1b<-s1bs<-ww0<-NULL
  b1<-1
  for(b in 1:B){
    Dat0<-DatL
    K<-sample(1:Nsub,1)
    {
      clust00<-sample(K,dim(Dat0)[2]-1,replace = TRUE)
      DatT0<-DatT     
      k0<-sample(1:4,1) 
      for(j in unique(clust00)){
        if(sum(clust00==j)<=1) next
        IonLb<-Dat0[,c(which(clust00==j))]
        IonLbT<-DatT0[,c(which(clust00==j))]
        pr<-c(knn(IonLb,IonLbT,k=k0,cl=as.factor(s0),prob = FALSE))
        if(length(unique(pr))==1) next
        s1b<-cbind(s1b,pr)
        
        ##############
        ns<-dim(Dat0)[1]*.2
        bn0<-sample(dim(Dat0)[1],ns,replace = FALSE)
        bn1<-setdiff(1:dim(Dat0)[1],bn0)
        IonLbs<-Dat0[bn1,c(which(clust00==j))]
        IonLbTs<-Dat0[bn0,c(which(clust00==j))]
        s0s<-ClassL[bn1]
        prs<-c(knn(IonLbs,  IonLbTs,k=k0,cl=as.factor(s0s),prob = FALSE))
        s1bs<-rbind(s1bs,mean(prs-ClassL[bn0]!=0))   
        
        
         b1<-b1+1
      }
    }
  }

  # s1b00<-s1b
  sel0<-which(s1bs<quantile(s1bs,probs = alpha))
  #  s1b00[,sel0]
  s1b1<-apply(s1b[,sel0],1,Mode)
  return(s1b1)
}

saggTC<-function(DatL,DatT, ClassL, B,Nsub,alpha){
  Dat0<-data.frame(cbind(DatL,ClassL))
  s1b<- s1bs<-NULL
  
  for(b in 1:B){
    K<-sample(1:Nsub,1)
    clust00<-sample(K,dim(Dat0)[2]-1,replace = TRUE)
    s1b0<-NULL
    for(j in unique(clust00)){
      if(sum(clust00==j)<=1) next
      IonLb<-data.frame(Dat0[,c(which(clust00==j),ncol(Dat0))])
      IonLbT<-data.frame(DatT[,c(which(clust00==j))])
      Ion.tr <- tree(ClassL ~ ., IonLb,mindev=1e-6, minsize=2)
      #    IonT<-Dat[T,rb]
      pr<-predict(Ion.tr,  IonLbT)
      s1b<-cbind(s1b,pr)
      
      ##############
      ns<-dim(Dat0)[1]*.2
      bn0<-sample(dim(Dat0)[1],ns,replace = FALSE)
      bn1<-setdiff(1:dim(Dat0)[1],bn0)
      IonLbs<-Dat0[bn1,c(which(clust00==j),ncol(Dat0))]
      IonLbTs<-Dat0[bn0,c(which(clust00==j))]
      
      Ion.tr <- tree(ClassL ~ ., IonLbs,mindev=1e-6, minsize=2)
      #    IonT<-Dat[T,rb]
      prs<-predict(Ion.tr,  IonLbTs)
      s1bs<-rbind(s1bs,mean(prs-ClassL[bn0]!=0))   
      
    }
  }
  
 # s1b00<-s1b
  sel0<-which(s1bs<quantile(s1bs,probs = alpha))
#  s1b00[,sel0]
  s1b1<-apply(s1b[,sel0],1,Mode)
  return(s1b1)

}
