\name{CreateSummaryMats2}
\alias{CreateSummaryMats2}
\title{helper function for \code{AddOnAllInd}}
\description{helper function for \code{AddOnAllInd}, not necessary for regular user}
\usage{
CreateSummaryMats2(AllDat, Directry)
}
\arguments{
  \item{AllDat}{not important see \code{\link{AddOnAllInd}}}
\item{Directry}{this the path where the folder specified by \code{AllNames} is located and default is working directory}
}
\details{see \code{AddOnAllInd}}
\value{
the direct output is not important, rather the function writes new .inp file and runs a .out file with \code{\link{MplusAutomation}} with all of the newly added indirect effects, some of which were deleted during backward selection
}
\references{M Plus}
\author{William Terry
}
\note{helper function no need to run by itself and function is called by \code{\link{AddOnAllInd}} which user should see for example}
\seealso{\code{\link{AddOnAllInd}}}
\examples{
#see AddOnAllInd which it is a helper function for
}
\keyword{Indirect Effects}
\keyword{helper}

