bagg<-function(DatL,DatT, ClassL, B,K, method){
  if(method=="KNN") {
    Re<-baggKNN(DatL,DatT,ClassL,B)
  }
  else{
    Re<-baggTC(DatL,DatT,ClassL,B)
  }
  return(Re)
}



baggKNN<-function(DatL,DatT,ClassL,B){
  s1b<-NULL
  L<-dim(DatL)[1]
  for(b in 1:B){
    Lb<-sample(L,replace=TRUE)
    IonLb<-DatL[c(Lb),]
    pr<-c(knn(IonLb, DatT, cl=as.factor(ClassL),prob = FALSE))
    s1b<-cbind(s1b,pr)
  }
  s1b1<-apply(s1b,1,Mode)
  return(s1b1)
}

baggTC<-function(DatL,DatT,ClassL,B){
  DatL<-cbind(DatL,ClassL)
  s1b<-NULL
  L<-dim(DatL)[1]
  for(b in 1:B){
    Lb<-sample(L,replace=TRUE)
    IonLb<-DatL[c(Lb),]
    Ion.tr <- tree(ClassL ~ ., IonLb,mindev=1e-6, minsize=2)
    pr<-predict(Ion.tr, DatT)
    s1b<-cbind(s1b,pr)
  }
  s1b1<-apply(s1b,1,Mode)
  return(s1b1)
}
