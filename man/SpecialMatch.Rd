\name{SpecialMatch}
\alias{SpecialMatch}
\title{Match and delete elements}
\description{used to delete superfluous spaces when parsing M Plus output}
\usage{
SpecialMatch(ListO, DelL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ListO}{
	\code{ListO} is list of lists which contain strings as each element}
  \item{DelL}{
	\code{DelL} is list of lists of same length as \code{ListO} and those elements which are NA are kept for ouput while 	all other elements are removed}
}
\details{see example}
\value{
  \item{}{input list of lists without elements specified by \code{DelL}}
}
\references{No references}
\author{William Terry}
\note{this is a helper function that does not need to be used by regular user}
\examples{
\dontshow{
SpecialMatch<-function(ListO,DelL){
	for(i in 1:length(ListO)){
		ListO[[i]]=ListO[[i]][is.na(DelL[[i]])==TRUE]
		}
	return(ListO)}
}
x=c()
x="    A$1                1.388      0.068     20.514      0.000"
x=c(x,"    B$1                1.858      0.139     13.340      0.000")
x=c(x,"    C$1                1.426      0.081     17.542      0.000")
x=c(x,"    D$1                1.644      0.092     17.934      0.000")
Thresh=x
AllThresh=sapply(Thresh,strsplit," ")
DL=lapply(AllThresh,match,"")
AllThreshNoSp=SpecialMatch(AllThresh,DL)
}
\keyword{Parse}
\keyword{helper}
