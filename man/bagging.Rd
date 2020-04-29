\name{bagging}
\alias{bagging}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Performs Bagging.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bagg(DatL,DatT, ClassL, B,method)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DatL}{Traning data set, An nx(J+1) data matrix; n is the number of observations and J is the number of dimensions. The Class variable should be defined undel name Class}
%%     ~~Describe \code{x} here~~
%}
  \item{DatT}{ Testing data set, An n0xJ data matrix; n0 is the number of observations and J is the number of dimensions.
%%     ~~Describe \code{B} here~~
}
 \item{ClassL}{ Label of testing data set, An 1xJ data matrix.
%%     ~~Describe \code{B} here~~
}

 \item{B}{Number of resampling}

 \item{method}{Classification method, method="KNN" or method="TC".}
}
\details{
The function predict the lable of the testing dat set via bagging, see Amiri (2016) for details.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{Amiri, S. (2016). Classification via Aggregating on variables.
}
\author{ Saeid Amiri.
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
library(tree)
library(class)

data('Sonar')
Class<-NULL
Class[Sonar[,ncol(Sonar)]=="R"]<-1
Class[Sonar[,ncol(Sonar)]=="M"]<-2
Sonar<-cbind(Sonar[,-ncol(Sonar)],Class)
Dat<-Sonar

  N<-dim(Dat)[1]
  T0<-N*.2
  L0<-N-T0
  K<-3

  T<-sample(N,T0,replace=FALSE)
  L<-setdiff(c(1:N),T)

  B<-10

  DatL0<-  Dat[L,-ncol(Dat)]
  DatT0<-  Dat[T,-ncol(Dat)]
  ClassL0<-Dat[L,ncol(Dat)]
  ClassT0<-Dat[T,ncol(Dat)]

  s1<-tree.class(DatL=DatL0,DatT=DatT0,ClassL=ClassL0)
  mean(abs(ClassT0-s1)!=0)

  s1<-bagg(DatL=DatL0,DatT=DatT0,ClassL=ClassL0,B,method="TC")
  mean(abs(ClassT0-s1)!=0)

  s1<-KNN.class(DatL=DatL0,DatT=DatT0,ClassL=ClassL0)
  mean(abs(ClassT0-s1)!=0)

  s1<-bagg(DatL=DatL0,DatT=DatT0,ClassL=ClassL0,B,method="KNN")
  mean(abs(ClassT0-s1)!=0)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
\keyword{classification}
