\name{CalculatRiskRatios}
\alias{CalculatRiskRatios}
\title{Calculate Risk Ratios from Direct Effects}
\description{calculates risk ratios from direct effects for each imputed data set path analyssi or just one data set path analysis if no imputations}
\usage{CalculatRiskRatios(AllDat, NADes, Directry = getwd(), WhichCat, WhichRiskCalc)}
\arguments{
  \item{AllDat}{output from \code{\link{ConvertData2}}}
  \item{NADes}{a one element vector containing a string which is the missing value designation}
  \item{Directry}{the working directory or the path where the folder for backward selection is located}
  \item{WhichCat}{list length of number of variables in initial data with 1 specifying variable is categorical and 0 specifying variable is continuous}
  \item{WhichRiskCalc}{list length of number of variables in initial data with 1 specifying use default for risk ratio calculation which is to calculate a risk ratio for all category levels compared to the lowest level and include an averaging over all of these levels when calculating other variable risk ratios and if variable is continuous 1 specifies using average of all values for variable compared to zero.  Otherwise a list can be given to specify which values to use for the risk ratios which are averaged over when calculated risk ratios for other variables.}
}
\details{if there is more than one threshold value for a categorical dependent variable the calculations are performed for all thresholds according to M Plus Manual Chapter 14 on probit calculations}
\value{
A dataframe where the row names are dependent variables and the columns are independent variables with the corresponding risk ratios. 
}
\references{M Plus method WLSMV}
\author{William Terry}
\note{averaging over possible combinations of levels of other variables in the risk ratio calculations is necessary since the direct effects are from a probit model using WLSMV in M Plus rather than from a logisitic regression model wich was not possible for categorical (non-binary) variables which are dependent and independent in path when this program was written}
\examples{
\dontrun{

InitD=Simulate(MissingYN=1)
xxx=Initialize(InitD,NumImpute=3,WhichCat=c(1,1,1,1,0))
ggg=AllBackwardSelect(xxx[[1]])
yyy=AllSummary2(xxx[[1]])
AllDat1=ConvertData2(xxx[[1]],xxx[[2]],yyy[[1]])
wwwA=lapply(AllDat1,CalculatRiskRatios,NADes=c(-99),WhichCat=c(1,1,1,1,0),WhichRiskCalc=c(0,
list(c(0,1,2)),list(c(0,1,2,4)),list(c(0,1,2)),0))
}
}
\keyword{Risk Ratios}
