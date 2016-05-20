\name{ParseTotalEffects}
\alias{ParseTotalEffects}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Parse Total, Direct, and Indirect Effects}
\description{read .out M Plus files to return total, indirect, and direct effect p-values and names of effects}
\usage{
ParseTotalEffects(OutFile, FileName, Directry)}
\arguments{
  \item{OutFile}{a read, scanned string of the .out M Plus file, see example}
  \item{FileName}{the name of the folder where the .out files are stored for backward selection}
  \item{Directry}{the working directory used to store the folders containing .out files}
}
\details{the Total effects and Total Indirect Effects results are returned reading IND statements output, and the Direct Effects results are returned by reading the Model ON statements}
\value{returns a list with the following objects:

  \item{TotalPVals}{A list where each element is the p-value of the Total Effect found under Model Indirect IND statements}
  \item{INDPVals}{A list where each element is the p-value of the Total Indirect Effect found under Model Indirect IND statements}
  \item{INDNames}{A matrix where each row contains the names of the variables found under Model Indirect IND statements corresponding to the Total and Total Indirect p-values respectively}	
  \item{DirectPVals}{A list where each element is the p-value of the Direct Effect found under Model created using ON statements}
  \item{DirectNames}{A matrix where each row contains the names of the variables found under Model ON statements corresponding to the Direct Effect p-values respectively}	
}
\references{no reference}
\author{William Terry}
\note{this function is a helper function used in \code{\link{AllBackwardSelect}} to determing which variable relations to delete}

\seealso{
See Also as \code{\link{AllBackwardSelect}}}
\examples{
\dontrun{

Simulated=Simulate(n=1000,MissingYN=0,exampleNum=2)
#MissingYN is 1 for add missing data 0 is default which is don't add missing data
uu=Initialize(Simulated,WhichCat=c(1,1,1,1,1,0,1,0,0),
DataFileName="Example2",NameFile="Example2D",Directry=getwd())
AB=AllBackwardSelect(uu[[1]],Directry=getwd())
x=scan(paste(getwd(),"/","Example2D","/","Example2D_","26",".out",sep=""),what=character())
gg=ParseTotalEffects(x,"Example2D",Directry=getwd())
}
}
\keyword{Parse}
\keyword{Total Effects}
\keyword{Indirect Effects}
\keyword{Direct Effects}