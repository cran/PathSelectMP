\name{AddOnAllInd}
\alias{AddOnAllInd}
\title{Indirect Effect Writer}
\description{wrapper function for creating indirect effects from direct effects and function writes indirect effects to file}
\usage{
AddOnAllInd(FileName, IndStatements, Directry = getwd())
}
\arguments{
  \item{FileName}{name without path of mplus .inp and .out files to read and write which is the base filename such as "New"}
  \item{IndStatements}{the indirect statements to write created by \code{\link{AddOnINDStatements}}}
  \item{Directry}{the path where all of the .inp and .out files are located}
}
\details{No Details required.}
\value{no value returned directly instead an extra .inp file is written and run with all IND effects}
\references{No references}
\author{William Terry}
\note{This function is a helper function for \code{\link{Initialize}} indirectly through \code{\link{WriteInitialInpFile}}.  Not used as a helper function is is useful only when PasteIND=0 options is utilized by \code{\link{Initialize}} due to having a very large number of variables, and then after backwards selection is performed, the user desires to add back to the model any possible indirect effects not in the model but possible with the final model.
}
\examples{
\dontrun{

InitD=Simulate(MissingYN=0,exampleNum=3)
xxx=Initialize(InitD,NumImpute=0,WhichCat=c(1,1,1,1,1,0,1,0),PasteIND=0)
ggg=AllBackwardSelect(xxx[[1]])
zzz=AllSummary(xxx[[1]])[[2]]
qqq=AddOnAllInd(xxx[[1]],zzz)
}
}
\keyword{Indirect Effects}
\keyword{helper}