\name{CreateSummaryMats}
\alias{CreateSummaryMats}
\title{Extract and Summarize Direct Effects}
\description{helper function for \code{AllSummary2} and not necessary for regular user}
\usage{CreateSummaryMats(FileName, OutputSE = FALSE, OutputPVal = FALSE, 
Directry, OutputFinalMat = TRUE)}
\arguments{
  \item{FileName}{string which is the name of the folder where the .inp and .out files are stored}
  \item{OutputSE}{TRUE outputs standard error dataframe and FALSE does not output standard error dataframe}
  \item{OutputPVal}{TRUE outputs p-value dataframe and FALSE does not p-value dataframe}
\item{Directry}{this the path where the folder specified by \code{AllNames} is located and default is working directory}
\item{OutputFinalMat}{a input value of 1 returns matrix and value of 0 returns a list}
}
\details{helper function}
\value{summaries of direct effects}
\references{M Plus}
\author{William Terry}
\note{no notes}
\seealso{\code{\link{AllSummary2}}}
\examples{
\dontrun{
InitD=Simulate(MissingYN=1)
xxx=Initialize(InitD,NumImpute=3,WhichCat=c(1,1,1,1,0))
ggg=AllBackwardSelect(xxx[[1]])
LL1=lapply(xxx[[1]],CreateSummaryMats,OutputSE=FALSE,
OutputPVal=FALSE,Directry=getwd(),OutputFinalMat=1)
}
}
\keyword{Summary}
\keyword{helper}
\keyword{Direct Effects}