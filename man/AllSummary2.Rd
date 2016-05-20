\name{AllSummary2}
\alias{AllSummary2}
\title{Direct Effect Summary}
\description{summarizes direct effects from path anaysis for all imputed datasets}
\usage{AllSummary2(AllNames, Directry = getwd(), GreaterThanNum = 0)}
\arguments{
  \item{AllNames}{\code{AllNames} is a list of the filenames as strings where the M Plus.inp and .out files are located which is length 1 if no imputations}
\item{Directry}{this the path where the folder specified by \code{AllNames} is located and default is working directory}
\item{GreaterThanNum}{number of output files containing path over which to average which must be less than the number of imputations and is only used if multiple imputations are performed. Default 0 which is to use all data sets in mean calculations.}
}
\details{this function is essentially the same as \code{AllSummary} except it does not return the indirect effect list}
\value{list of length 2:
  \item{DirectEffects}{Direct Effects of each imputed data set or just direct effects of one data set if no imputation}
  \item{Average}{list with the following objects:}	
       \item{DirectEffectCounts}{count matrix for number of times path appears which will be 1's and 0's if no imputed data sets are used}
       \item{MeanDirectEffects}{mean values of direct effects for paths which are just the direct effects if no imputations are performed}
       \item{MeanStandardError}{the mean square error of the effect parameters averaged over imputed data sets if they exist}
       \item{MeanPValue}{mean p values of these direct effects}
       \item{MinPVals}{minimum p values of these direct effects}
       \item{MaxPVals}{maximum p value of these direct effects}
       \item{MedianPVals}{median p value of these direct effects}
	
}
\references{M Plus}
\author{William Terry}
\note{Must initialize and run backwards selection before using this function}
\seealso{\code{\link{AllSummary}}}
\examples{
\dontrun{

InitD=Simulate(MissingYN=1)
xxx=Initialize(InitD,NumImpute=3,WhichCat=c(1,1,1,1,0))
ggg=AllBackwardSelect(xxx[[1]])
yyy=AllSummary2(xxx[[1]])

}
}
\keyword{summary}