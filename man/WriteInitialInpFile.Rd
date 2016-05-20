\name{WriteInitialInpFile}
\alias{WriteInitialInpFile}
\title{Write Inp File}
\description{function writes first M Plus .inp files for each imputed data set or just one file if no imputed data sets}
\usage{
WriteInitialInpFile(AllData, InitialData, InputInitializeMat, IndList, 
Directry, NADes, WhichCat, WhichImpute, WhichRowsImp, AllMethods)}
\arguments{
  \item{AllData}{list of length three with the first element a list of strings with each string as the name of the folder where .inp and .out files are written, the second element is a list of strings with each string the name of the .dat file containing imputed data set or one string name of data set if no imputations, and the third element is a list of numbers of the imputation seed used by \code{MICE} for imputation which is empty if there are no imputed data sets}
  \item{InitialData}{dataframe of initial data}
  \item{InputInitializeMat}{square dataframe where row name and column names are equal and are the variable names of InitialData with 1 for include path and 0 for exclude created by \code{\link{CreateInitializeMatrix}}}
  \item{IndList}{the indirect statements to write created by \code{AddOnINDStatements}}
  \item{Directry}{list of length number of variables in InitialData with 1 for categorical and 0 for continuous variables}
  \item{NADes}{a list of length 1 with the value for missing data}
  \item{WhichCat}{list of length number of variables in InitialData with 1 for categorical and 0 for continuous variables}
  \item{WhichImpute}{list of length of names of InitialData specifying '1' for Impute column and '0' for no Imputation}
  \item{WhichRowsImp}{list of length of number of rows of InitialData specifying '1' for Impute row and '0' for no Imputation}
  \item{AllMethods}{list of length 3 for \code{MICE} imputation for binary, unordered catagorical, and continuous imputations default in \code{Initialize} set as"logreg" for binary imputation "polr" for categorical imputation "pmm" for continuous}
}
\details{this is a helper function for \code{Initialize} and should not be used by regular user}
\value{output is string which is the .inp file which is to be written to file using \code{Initialize} function}
\references{\code{MPlusAutomation} \code{MICE}}
\author{William Terry}
\note{helper function. Also note that the input file in this version should not be modified by the user since some of the line numbers are set for reading and should not be altered}
\seealso{\code{\link{Initialize}}}
\examples{
\dontrun{
InitD=Simulate(MissingYN=1)
NumImpute=3
startSeedImputations=1000
DataFileName="NewData"
NameFile="New"
Directry=getwd()
NADes=c(-99)
WhichCat=c(1,1,1,1,0)
AllMethods=c("logreg","polr","pmm")
WhichImpute=rep(1,ncol(InitD))
WhichRowsImp=c(1:nrow(InitD))
NumImpList=seq(startSeedImputations,(startSeedImputations+NumImpute-1),1)
NameFile=paste0(NameFile,"_")
DataFileName=paste0(DataFileName,"_")
AllData=ConvertData3(NameFile,DataFileName,NumImpList)
InputInitializeMat=CreateInitializeMatrix(InitD,WhichCat,empty=FALSE)
IndList=AddOnINDStatements(InputInitializeMat,PasteIND=1)
WRt=lapply(AllData,WriteInitialInpFile,InitD,InputInitializeMat,IndList,Directry,
NADes,WhichCat,WhichImpute,WhichRowsImp,AllMethods)
}
}
\keyword{Initialize}
\keyword{helper}