\name{Sonar}
\alias{Sonar}
\docType{data}
\title{Sonar data
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("Sonar")}
\format{
  The format of the data is a dataframe.
}
\details{
A simple classification data set containing different patterns obtained by bouncing sonar
     signals off a metal cylinder at various angles and under various
       conditions.  It includes 208 observations and 60  variables and two classes,.
}
\source{
Lichman, M. (2013). UCI Machine Learning Repository [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California, School of Information and Computer Science.

\url{https://archive.ics.uci.edu/ml/datasets/Urban+Land+Cover}

 Gorman, R. P., & Sejnowski, T. J. (1988). Analysis of hidden units in a layered network trained to classify sonar targets. Neural networks, 1(1), 75-89.

}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data('Sonar')
Class<-NULL
Class[Sonar[,ncol(Sonar)]=="R"]<-1
Class[Sonar[,ncol(Sonar)]=="M"]<-2
Sonar<-cbind(Sonar[,-ncol(Sonar)],Class)
head(Sonar)
}
\keyword{datasets}
