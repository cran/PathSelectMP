\name{CreateTotalSummary}
\alias{CreateTotalSummary}
\title{Summarizes and Averages Total Effects}
\description{this is a helper function for \code{AllTotEffOutput} which is better for the regular user}
\usage{CreateTotalSummary(AllTots, GreaterThanNum)}
\arguments{
  \item{AllTots}{is output from \code{CreateTotalEffMat} applied to each imputed data sets so it is a list of length number of imputed data sets (or length 1 if no imputed data sets) cotaining in each list a list of dataframes where each data frame is total effects, standard errors, p-values, path designation respectively
}
  \item{GreaterThanNum}{number of output files containing path over which to average which must be less than the number of imputations and is only used if multiple imputations are performed. }
}
\details{not necessary for regular user}
\value{output is same as \code{\link{AllTotalEffOutput}} which is easier to run}
\references{M Plus}
\author{William Terry}
\note{must run backwards selection first and some other necessary functions see example}
\seealso{\code{\link{AllTotalEffOutput}}}
\examples{
\dontrun{


InitD=Simulate(MissingYN=1)
xxx=Initialize(InitD,NumImpute=3,WhichCat=c(1,1,1,1,0))
ggg=AllBackwardSelect(xxx[[1]])
yyy=AllSummary2(xxx[[1]])
zzz=AllSummary(xxx[[1]])[[2]]
qqq=AddOnAllInd(xxx[[1]],zzz)
AllDat1=ConvertData2(xxx[[1]],xxx[[2]],yyy[[1]])
AllFileNames1=sapply(AllDat1,OnlyNumberElement,1)
Tots1=lapply(AllFileNames1,CreateTotalEffMat,Directry=getwd())	
AllTotEffOutput1=CreateTotalSummary(Tots1,GreaterThanNum=0)
}
}
\keyword{Summary}
