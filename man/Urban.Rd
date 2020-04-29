\name{Urban}
\alias{Urban}
\docType{data}
\title{Urban data
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("Urban")}
\format{
  The format of the data is a dataframe.
}
\details{
A simple classification data set containing  a high resolution aerial image into 9 types of urban land cover.  Here we consider the training data set, it includes 147 variables and  n=168 and also has 9 classes.
this data can be used to evaluate clustering methods.
}
\source{
Lichman, M. (2013). UCI Machine Learning Repository [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California, School of Information and Computer Science.



\url{https://archive.ics.uci.edu/ml/datasets/Urban+Land+Cover}
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data('Urban')
Class<-as.numeric(Urban$class)
Urban<-cbind(Urban[,-1],Class)
Dat<-Urban
}
\keyword{datasets}
