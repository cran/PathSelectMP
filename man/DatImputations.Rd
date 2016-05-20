\name{DatImputations}
\alias{DatImputations}
\title{Impute Data}
\description{Imputes missing data using \link[mice]{mice}}
\usage{
DatImputations(InitialData, ImputeSeed, NADes, DataFileNameS, 
WhichCat, WhichImpute, WhichRowsImp, AllMethods)
}
\arguments{
  \item{InitialData}{dataframe of data to be used in model creation}
  \item{ImputeSeed}{the seed for imputations}
  \item{NADes}{the missing value designation}
  \item{DataFileNameS}{name of imputed data file}
  \item{WhichCat}{vector length of names of InitialData specifying '1' for categorical and '0' for continuous variable}
  \item{WhichImpute}{vector length of names of InitialData specifying '1' for Impute column and '0' for no Imputation}
  \item{WhichRowsImp}{vector length of number of rows of InitialData specifying '1' for Impute row and '0' for no Imputation}
  \item{AllMethods}{default is set at "logreg" for binary imputation "polr" for categorical imputation "pmm" for continuous}
}
\details{this is a helper function for \code{\link{Initialize}} when imputations are desired by user, and it should not be used as standalone function by regular user}
\value{No value is returned rather a new data set is saved}
\references{\code{\link[mice]{mice}} https://cran.r-project.org/web/packages/mice/mice.pdf}
\author{William Terry}
\note{This function is best used by specificying the number of desired imputations with NumImpute in \code{\link{Initialize}}}
\seealso{\code{\link[mice]{mice}} https://cran.r-project.org/web/packages/mice/mice.pdf}

\examples{
\dontrun{

InitD=Simulate(MissingYN=1,exampleNum=1)
WhichCat=c(1,1,1,1,0)
WhichImpute=c(rep(1, ncol(InitD)))
WhichRowsImp=c(rep(1,nrow(InitD)))
AllMethods=c("logreg","polr","pmm")
Directry=getwd()
ImputeSeed=1000
TO=Directry
DataFileName="NewData_1000"
NameFile="New_1000"
TO=paste(TO,"/",NameFile,sep="")
DataFileNameS=paste0(TO,"/",DataFileName,".dat")
NADes=c(-99)
Initialize(InitD,NumImpute=1,WhichCat=c(1,1,1,1,0))
DatImputations(InitD,ImputeSeed,NADes,DataFileNameS,WhichCat,WhichImpute,WhichRowsImp,AllMethods)
}
}

\keyword{Impute}
\keyword{helper}

