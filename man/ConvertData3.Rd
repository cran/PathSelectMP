\name{ConvertData3}
\alias{ConvertData3}
\title{Used to make multiple list names for imputations}
\description{uses imputation seed numbers and names of data to create names of imputed data, helper function}
\usage{ConvertData3(N1, N2, List1)}
\arguments{
  \item{N1}{name of file}
  \item{N2}{name of data file}
  \item{List1}{list of imputation seeds}
}
\details{regular user doesn't need this function but it useful for creating new imputation folder names}
\value{returns list of new folders for imputed analysis}
\references{no reference}
\author{William Terry}
\note{helper function}
\examples{
\dontshow{
ConvertData3<-function(N1,N2,List1){
		NewList=c()
		for(i in 1:length(List1)){
			NewList=c(NewList,list(c((paste0(N1,List1[i])),(paste0(N2,List1[i])),c((List1[i])))))
			}
		return(NewList)}
}
NumImpute=3
startSeedImputations=1000
NumImpList=seq(startSeedImputations,(startSeedImputations+NumImpute-1),1)
DataFileName="NewData"
NameFile="New"
NameFile=paste0(NameFile,"_")
DataFileName=paste0(DataFileName,"_")
AllData=ConvertData3(NameFile,DataFileName,NumImpList)
}
\keyword{helper}

