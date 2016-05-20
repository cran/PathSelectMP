\name{MPlusBackwardSelect}
\alias{MPlusBackwardSelect}
\title{Backward Selection Helper Function}
\description{performs backward selection for \code{\link{AllBackwardSelect}}}
\usage{MPlusBackwardSelect(FileName, Directry, PSig)}
\arguments{
  \item{FileName}{name of folder where .inp and .out files saved}
  \item{Directry}{the path where folder is located containing .inp and .out M Plus files}
  \item{PSig}{the significance level used for backward selection}
}
\details{the regular user should not use this function and is referred to instead use the wrapper function code{\link{AllBackwardSelect}} which calls this function}
\value{does not return value but rather reads and writes .inp and .out files to specified folder and directory}
\references{M Plus}
\author{William Terr}
\note{see \code{\link{AllBackwardSelect}}}
\examples{
\dontrun{

InitD=Simulate()
xxx=Initialize(InitD)
ggg1=MPlusBackwardSelect(xxx[[1]],getwd(),0.05)
}
}
\keyword{Variable Selection}
\keyword{helper}