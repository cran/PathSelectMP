\name{OnlyNumberElement}
\alias{OnlyNumberElement}
\title{indexes list of lists}
\description{simple helper function to return only specified list element of set of lists to be used with apply functions}
\usage{OnlyNumberElement(AllData, Number)}
\arguments{
  \item{AllData}{list of lists}
  \item{Number}{index number of list element to return}
}
\details{created for easy indexing of lists of lists with apply functions}
\value{
  \item{comp1 }{designated list element}
}
\references{No references}
\author{William Terry}
\note{No notes}
\examples{
\dontshow{
OnlyNumberElement<-function(AllData,Number){
	return(AllData[[Number]])}
}
Dat=list(c(list("new1"),list("new2")),c(list("Old1"),list("Old2")))
AllFileNames1=sapply(Dat,OnlyNumberElement,1)
}

\keyword{Index}
\keyword{helper}

