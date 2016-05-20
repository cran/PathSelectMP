\name{AllSummary}
\alias{AllSummary}
\title{Direct Effects Path Summaries}
\description{One of the main and most important functions.  Ties together Indirect Statements and summary output of direct effects}
\usage{AllSummary(AllNames, Directry = getwd(), GreaterThanNum = 0, PasteIND = 1)}
\arguments{
  \item{AllNames}{\code{AllNames} is the filename where the M Plus.inp and .out files are located}
  \item{Directry}{this the path where the folder specified by \code{AllNames} is located and default is working directory}
  \item{GreaterThanNum}{number of output files containing path over which to average which must be less than the number of imputations and is only used if multiple imputations are performed. Default 0 which is to use all data sets in mean calculations.}
  \item{PasteIND}{a value of 1 indicates to use all possible indirect effects in modelling and a value of 0 is input to only use direct effects in modelling}
}
\details{Must initialize and run backwards selection before using this function}
\value{
    \item{Average}{list with the following objects:}	
       \item{DirectEffectCounts}{count matrix for number of times path appears which will be 1's and 0's if no imputed data sets are used}
       \item{MeanDirectEffects}{mean values of direct effects for paths which are just the direct effects if no imputations are performed}
       \item{MeanStandardError}{the mean square error of the effect parameters averaged over imputed data sets if they exist}
       \item{MeanPValue}{mean p values of these direct effects}
       \item{MinPVals}{minimum p values of these direct effects}
       \item{MaxPVals}{maximum p value of these direct effects}
       \item{MedianPVals}{median p value of these direct effects}
  \item{INDStatements}{lists of indirect effect relations}
}
\references{M Plus}
\author{William Terry}
\note{This function must be run before \code{\link{AddOnAllInd}} can be run (see examples), but otherwise is not a very useful summary function. The user is
instead referred to \code{\link{AllSummary2}}. The means in the matrices above are only calculated for those paths and parameters which appear in the count matrix with a value greater than the GreaterThanNum.}
\seealso{\code{\link{Initialize}} \code{\link{AllBackwardSelect}} }
\examples{
\dontrun{

InitD=Simulate(MissingYN=1)
xxx=Initialize(InitD,NumImpute=3,WhichCat=c(1,1,1,1,0))
ggg=AllBackwardSelect(xxx[[1]])
zzz=AllSummary(xxx[[1]])[[2]]
}
}
\keyword{Summary}