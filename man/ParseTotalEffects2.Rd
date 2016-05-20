\name{ParseTotalEffects2}
\alias{ParseTotalEffects2}
\title{Parse and Exctract Total Effects}
\description{finds names and values of total effects and returns as lists}
\usage{
ParseTotalEffects2(OutFile, StandardError = FALSE, PVal = FALSE, Indirect = FALSE)
}
\arguments{
  \item{OutFile}{name of folder where .inp and .out files are located}
  \item{StandardError}{if TRUE then standard errors are returned default is false and if also PVal is false then effects returned}
  \item{PVal}{if TRUE then p values for total effects are returns default is FALSE}
  \item{Indirect}{if 1 then total indirect effects are returned if 0 then total effects are returned}
}
\details{helper function for \code{\link{TotalRiskRatios}}, \code{\link{CreateTotalEffMat}} and all other total effect functions and does not need to be used by regular user}
\value{
\item{TotalVals}{1st list is values returned either total effects or standard errors of total effects or p values of total effects}
\item{TotalEffectNames}{list of names of total effects}
}
\references{M Plus}
\author{William Terry}
\note{helper function, but useful for parsing if modifying package or certain functions}
\seealso{\code{\link{TotalRiskRatios}} and \code{\link{CreateTotalEffMat}}}
\examples{
\dontrun{

InitD=Simulate(MissingYN=0,exampleNum=3)
xxx=Initialize(InitD,NumImpute=0,WhichCat=c(1,1,1,1,1,0,1,0),PasteIND=1)
ggg=AllBackwardSelect(xxx[[1]])
AllDat1=ConvertData2(xxx[[1]],xxx[[2]],yyy[[1]])
Directry=getwd()
LastFileOut=AllDat1[[1]][[1]]
TO=paste(Directry,"/",LastFileOut,sep="")
files <- list.files(path=TO,pattern = ".out$")
hh=lapply(strsplit(files,"_"),NumEndFile,pattern1=".out",pattern2='out')
hh=as.numeric(paste(hh))
LastFileName=files[which(hh==max(hh))]
x=scan(paste(TO,"/",LastFileName,sep=""),what=character())
GG=ParseTotalEffects2(x)
}
}
\keyword{Parse}