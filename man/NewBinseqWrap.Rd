\name{NewBinseqWrap}
\alias{NewBinseqWrap}
\title{Possible Combinations of Elements, 1 from each list}
\description{creates a list of lists where each list element has n elements 1 element from each of the first n lists}
\usage{
NewBinseqWrap(n, PossibleCoefs)
}
\arguments{
  \item{n}{an integer less than or equal to the number of lists used to choose from which specifies the length of each combination elements from the list choices
}
  \item{PossibleCoefs}{a list of lists where one element at a time will be combined with elements of the other list}
}
\details{this is a helper function and not necessary for the regular user}
\value{returns the list of lists for possible combinations of 1 element at a time from each list}
\references{No references}
\author{William Terry}
\note{The global variable V is used to return the list and thus V should not be used for any other variable name.  This may need to be corrected in future versions}
\examples{
\dontrun{

NewBinseqWrap(n=3,PossibleCoefs=c(list(c(0,1,2)),list(c(0,1,2,3)),list(c(4,5,6))))
}
}
\keyword{Risk Ratios}
\keyword{helper}
