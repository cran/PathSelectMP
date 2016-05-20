\name{ConvertData2}
\alias{ConvertData2}
\title{Converts Necessary Inputs for \code{\link{CalculatRiskRatios}}}
\description{converts inputs to list structure}
\usage{ConvertData2(List1, List2, jjlist1)}
\arguments{
  \item{List1}{list of strings where each string is the name of the folder for each imputed backward selection or just one name if no imputation}
  \item{List2}{list of strings here each string is the name of the .dat file (without .dat in string) for the imputed data set or just one .dat file name if no imputation}
  \item{jjlist1}{list of dataframes where each dataframe is the direct effects dataframe from backward selection with the imputed dataset which is only a list of length 1 with one matrix of direct effects if no imputations}
}
\details{follow example and in future versions a wrapper function will be written}
\value{list of length number of imputed data sets (length 1 if no imputations) containing three elements in each list.  The first element is the name of the imputed selection folder contained in \code{List1}, the second element is the name of the dataset contained in \code{List2}, and the 3rd element is the dataframe of direct effects in \code{jjlist1}}
\references{No references}
\author{William Terry}
\note{no notes}
\seealso{\code{\link{CalculatRiskRatios}}}
\examples{
\dontrun{

InitD=Simulate(MissingYN=1)
xxx=Initialize(InitD,NumImpute=3,WhichCat=c(1,1,1,1,0))
ggg=AllBackwardSelect(xxx[[1]])
yyy=AllSummary2(xxx[[1]])
AllDat1=ConvertData2(xxx[[1]],xxx[[2]],yyy[[1]])
}
}
\keyword{Risk Ratios}
\keyword{helper}
