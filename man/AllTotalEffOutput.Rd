\name{AllTotalEffOutput}
\alias{AllTotalEffOutput}
\title{Summary of Total Effects}
\description{total effects averaged over imputed data sets, if there are imputations, with accompanying p value summaries}
\usage{
AllTotalEffOutput(AllDat, GreaterThanNum = 0, Directry = getwd())
}
\arguments{
  \item{AllDat}{output from \code{\link{ConvertData2}}}
  \item{GreaterThanNum}{number of output files containing path over which to average which must be less than the number of imputations and is only used if multiple imputations are performed. Default 0 which is to use all data sets in mean calculations.}
  \item{Directry}{this the path where the folder specified by \code{AllNames} is located and default is working directory}
}
\details{Must initialize and run backwards selection before using this function}
\value{list of matrices which are respectively:
  \item{Count}{number of times total effect appears in imputed data sets final path selection}
  \item{AverageEffects}{mean values of total effects for paths which are just the total effects if no imputations are performed}
  \item{AverageStandardError}{the mean square error of the effect parameters averaged over imputed data sets if they exist}
  \item{AveragePVal}{mean p values of these total effects}
  \item{MinPval}{minimum p values of these total effects}
  \item{MaxPVal}{maximum p value of these total effects}
  \item{MedianPVal}{median p value of these total effects}
}
\references{M Plus}
\author{William Terry}
\note{total effects and direct effect averages are done separately so a path may be present in one summary and not the other when using the same \code{GreaterThanNum} which is not equal to zero}
\seealso{\code{\link{AllSummary2}}}
\examples{
\dontrun{

InitD=Simulate(MissingYN=1)
xxx=Initialize(InitD,NumImpute=3,WhichCat=c(1,1,1,1,0))
ggg=AllBackwardSelect(xxx[[1]])
yyy=AllSummary2(xxx[[1]])
AllDat1=ConvertData2(xxx[[1]],xxx[[2]],yyy[[1]])
TotsAllEff=AllTotalEffOutput(AllDat1)
}
}
\keyword{Total Effects}
