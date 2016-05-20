\name{AverageRRs}
\alias{AverageRRs}
\title{Calculate Average Risk Ratios over all imputed data sets}
\description{Averages the risk ratio values for all paths in imputed data sets if the specific path relationship appears frequently enough as specified by user}
\usage{AverageRRs(ListORatioMats, GreaterThanCountNum)}
\arguments{
\item{ListORatioMats}{a list of dataframe where each dataframe is the risk ratios matrix calculated for each imputed data set
}
  \item{GreaterThanCountNum}{the number of times a path relationship should appear more times than in order to be included in averaging of risk ratios for this relationship default is 0}
}
\details{adds risk ratios for relationship and divides by number of times this relationship appears as long as the relationship appears more than \code{GreaterThanCountNum}}
\value{
  \item{AverageRiskRatios}{data frame consisting of averaged risk ratios}
  \item{CountInAverage}{same data frame as \code{AverageRiskRatios} except in place of averaged risk ratio is corresponding count number for path relationship over all imputed data sets}
}
\references{no references}
\author{William Terry}
\note{no notes}
\examples{
\dontrun{

InitD=Simulate(MissingYN=1)
xxx=Initialize(InitD,NumImpute=3,WhichCat=c(1,1,1,1,0))
ggg=AllBackwardSelect(xxx[[1]])
yyy=AllSummary2(xxx[[1]])
AllDat1=ConvertData2(xxx[[1]],xxx[[2]],yyy[[1]])
wwwA=lapply(AllDat1,CalculatRiskRatios,NADes=c(-99),WhichCat=c(1,1,1,1,0),WhichRiskCalc=c(0,
list(c(0,1,2)),list(c(0,1,2,4)),list(c(0,1,2)),0))
AvgwwwA=AverageRRs(wwwA,0)

RRT1=lapply(AllDat1,TotalRiskRatios,InputDepVal=1)
RRT1avg=AverageRRs(RRT1,0)
}
}
\keyword{Risk Ratios}
