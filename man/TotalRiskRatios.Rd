\name{TotalRiskRatios}
\alias{TotalRiskRatios}
\title{Calculate Risk Ratios for total effects}
\description{Risk Ratios for the total effects are calculated using threshold values obtained for direct effects and the total effect values see in \code{AllTotalEffOutput}}
\usage{TotalRiskRatios(AllDat, Directry = getwd(), InputDepVal = 1)}
\arguments{
 \item{AllDat}{output from \code{\link{ConvertData2}}}
 \item{Directry}{this the path where the folder specified by \code{AllNames} is located and default is working directory}
  \item{InputDepVal}{a number which is the value of the dependent variable used for risk ratio which is compared to 0 in risk ratio calculation, default value is 1}
}
\details{total effects are sum of direct and indirect effects where indirect effects are the product of all direct effects in the chain which comprises the indirect variable chain, this total effect value is converted to its probability using the threshold values for direct effects and multiplying the total effect with the specified input value}
\value{returns data frame with risk ratios in place of total effects}
\references{M Plus}
\author{William Terry}
\note{of course backwards selection and other functions must run first, see example}
\seealso{code{\link{AllTotalEffOutput}}, code{\link{AverageRRs}}}
\examples{
\dontrun{

InitD=Simulate(MissingYN=1)
xxx=Initialize(InitD,NumImpute=3,WhichCat=c(1,1,1,1,0))
ggg=AllBackwardSelect(xxx[[1]])
yyy=AllSummary2(xxx[[1]])
AllDat1=ConvertData2(xxx[[1]],xxx[[2]],yyy[[1]])
RRT1=lapply(AllDat1,TotalRiskRatios,InputDepVal=1)
}
}
\keyword{Risk Ratios}
\keyword{Total Effects}

