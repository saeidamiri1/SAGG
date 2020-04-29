saggKNN<-function(DatL,DatT, ClassL, B,KK){

  DD<-scale(rbind(DatL,DatT))
  DatL<-scale(DatL, center = attr(DD,"scaled:center"), scale = attr(DD,"scaled:scale"))
  DatT<-scale(DatT, center = attr(DD,"scaled:center"), scale = attr(DD,"scaled:scale"))
  s0<-ClassL
  s1b<-ww0<-NULL
  b1<-1
  for(b in 1:B2){
    Dat0<-DatL
    K<-sample(1:KK,1)
    {
      clust00<-sample(K,dim(Dat0)[2]-1,replace = TRUE)
      DatT0<-DatT     
      k0<-sample(1:4,1) 
      for(j in unique(clust00)){
        if(sum(clust00==j)<=1) next
        IonLb<-Dat0[,c(which(clust00==j))]
        IonLbT<-DatT0[,c(which(clust00==j))]
        pr<-c(knn(IonLb,  IonLbT,k=k0,cl=as.factor(s0),prob = FALSE))
        if(length(unique(pr))==1) next
        s1b<-cbind(s1b,pr)
        b1<-b1+1
      }
    }
  }
  s1b00<-s1b
  s1b1<-apply(s1b00,1,Mode)
  return(s1b1)
}

saggTC<-function(DatL,DatT, ClassL, B,KK){
  Dat0<-cbind(DatL,ClassL)
  s1b<-NULL
  
  for(b in 1:B){
    K<-sample(1:KK,1)
    clust00<-sample(K,dim(Dat0)[2]-1,replace = TRUE)
    s1b0<-NULL
    for(j in unique(clust00)){
      if(sum(clust00==j)<=1) next
      IonLb<-Dat0[,c(which(clust00==j),ncol(Dat0))]
      IonLbT<-DatT[,c(which(clust00==j))]
      Ion.tr <- tree(ClassL ~ ., IonLb,mindev=1e-6, minsize=2)
      #    IonT<-Dat[T,rb]
      pr<-predict(Ion.tr,  IonLbT)
      s1b<-cbind(s1b,pr)
    }
  }
  
  s1b1<-apply(s1b,1,Mode)
  return(s1b1)
}
