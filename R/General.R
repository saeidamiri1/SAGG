## Find Mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

tree.class<-function(DatL,DatT,ClassL){
  DatL<-cbind(DatL,ClassL)
  Ion.tr <- tree(ClassL ~ ., DatL,mindev=1e-6, minsize=2)
  pr<-predict(Ion.tr, DatT)
  return(as.vector(pr))
}


KNN.class<-function(DatL,DatT,ClassL){
  pr<-c(knn(DatL, DatT,cl=as.factor(ClassL),prob = FALSE))
  return(as.vector(pr))
}


sagg<-function(DatL,DatT, ClassL, B,K, method){
  if(method=="KNN") {
    Re<-saggKNN(DatL,DatT, ClassL, B,K)
  }
  else{
    Re<-saggTC(DatL,DatT, ClassL, B,K)
  }
  return(Re)
}

