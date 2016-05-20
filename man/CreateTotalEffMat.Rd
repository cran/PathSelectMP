\name{CreateTotalEffMat}
\alias{CreateTotalEffMat}
\title{Summarizes Total Effects}
\description{this is a helper function for \code{AllTotEffOutput1} which is better for the regular user and it is a helper function for \code{CreateTotalSummary}, and it is used to extract total effects from M Plus .out files}
\usage{CreateTotalEffMat(FileName, Directry)}
\arguments{
  \item{FileName}{a list of length 1 containing the string which is the folder name which contains .inp and .out M Plus files after backwards selection
}
  \item{Directry}{the path where the folder specified by 
\code{FileName} is located}
}
\details{extracts total effects with standard errors and p values from final .out file}
\value{returns list of length 4:
\item{TotalEffects}{dataframe containing the total effects}
\item{TotalEffectsStandardError}{data frame with the standard errors of the total effects}
\item{TotalEffectsPVals}{dataframe contains the p values for these effects}
\item{TotalEffectsCount}{data frame with a 0 if the total effect is not present and a 1 if it is present in the analysis}
}
\references{M Plus}
\author{William Terry}
\note{this function is applied over all imputed data sets or can be used with one data set and analysis at a time, but this function is a helper function that is likely not necessary for the regular user}
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
Tots=CreateTotalEffMat(AllFileNames1[1],Directry=getwd())
}
}
\keyword{Summary}
\keyword{Total Effects}
