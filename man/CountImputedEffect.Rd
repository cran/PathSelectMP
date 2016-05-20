\name{CountImputedEffect}
\alias{CountImputedEffect}
\title{Count and Average Effects for All Imputed Datasets}
\description{averages direct effects and corresponding standard errors and p values for results from each imputed data set}
\usage{
CountImputedEffect(LL, LLse, LLPVal, GreaterThanNum)
}
\arguments{
  \item{LL}{list containing square matrices as each element, 1 matrix for each imputed data set, with the final direct effects}
  \item{LLse}{list containing square matrices as each element, 1 matrix for each imputed data set, with the final stander errors for direct effects}
  \item{LLPVal}{list containing square matrices as each element, 1 matrix for each imputed data set, with the final p values for direct effects}
  \item{GreaterThanNum}{an integer less than the number of imputed data sets representing the number of times a path must be counted more than this number of times in order to be included in averaging of effects, effect standard errors, and effect p values}
}
\details{
this function is a helper function and does not need to be used by the regular user
}
\value{list which contains the following objects:
  \item{DirectEffectCounts}{count matrix for number of times path appears which will be 1's and 0's if no imputed data sets are used}
  \item{MeanDirectEffects}{mean values of direct effects for paths which are just the direct effects if no imputations are performed}
  \item{MeanStandardError}{the mean square error of the effect parameters averaged over imputed data sets if they exist}
  \item{MeanPValue}{mean p values of these direct effects}
  \item{MinPVals}{minimum p values of these direct effects}
  \item{MaxPVals}{maximum p value of these direct effects}
  \item{MedianPVals}{median p value of these direct effects}
}
\references{no references}
\author{William Terry}
\note{No notes}
\seealso{\code{\link{AllSummary}}}
\examples{
\dontrun{

InitD=Simulate()
xxx=Initialize(InitD)
ggg=AllBackwardSelect(xxx[[1]])
AllNames=xxx[[1]]
LL1=lapply(AllNames,CreateSummaryMats,OutputSE=FALSE,OutputPVal=FALSE,Directry=getwd())
LL1se=lapply(AllNames,CreateSummaryMats,OutputSE=TRUE,OutputPVal=FALSE,Directry=getwd())
LL1p=lapply(AllNames,CreateSummaryMats,OutputSE=FALSE,OutputPVal=TRUE,Directry=getwd())
LLL=CountImputedEffect(LL1,LL1se,LL1p,GreaterThanNum=0)
}
}
\keyword{Summary}
\keyword{helper}
\keyword{Direct Effects}

