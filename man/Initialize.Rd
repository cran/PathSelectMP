\name{Initialize}
\alias{Initialize}
\title{Create Initial Objects}
\description{write data files and initial .inp files to setup file directory system for running backwards selection using M Plus}
\usage{
Initialize(InitialData, NumImpute = 0, DataFileName = "NewData", 
NameFile = "New", Directry = getwd(), 
NADes = c(-99), startSeedImputations = 1000, 
InputInitializeMat = "N", WhichCat = rep(1, ncol(InitialData)), 
AllMethods = c("logreg", "polr", "pmm"), WhichImpute = rep(1, ncol(InitialData)), 
WhichRowsImp = c(1:nrow(InitialData)), PasteIND = 1)
}
\arguments{
  \item{InitialData}{dataframe of initial data}
  \item{NumImpute}{the number of imputated datasets to create default is 0 for no imputation}
  \item{DataFileName}{the name of the .dat file to be created from the initial data or imputed datasets default is "NewData"}
  \item{NameFile}{the name of the folder where the .inp and .out files with the same base name are stored default is "New"}
  \item{Directry}{the path where the folder containing the .inp .out and .dat files are saved during backward selection}
  \item{NADes}{a list of length 1 with the value for missing data default is c(-99)}
  \item{startSeedImputations}{the seed for the first imputed data sets all other data sets seed is plus one from this value default is 1000}
  \item{InputInitializeMat}{square dataframe where row name and column names are equal and are the variable names of InitialData with 1 for include path and 0 for exclude created by \code{\link{CreateInitializeMatrix}} default creates upper triangular data frame}
  \item{WhichCat}{list of length number of variables in InitialData with 1 for categorical and 0 for continuous variables default is all 1s for all categorical data}
  \item{AllMethods}{default is set at "logreg" for binary imputation "polr" for categorical imputation "pmm" for continuous}
  \item{WhichImpute}{list of length of names of InitialData specifying '1' for Impute column and '0' for no Imputation}
  \item{WhichRowsImp}{list of length of number of rows of InitialData specifying '1' for Impute row and '0' for no Imputation}
  \item{PasteIND}{a value of 1 indicates to use all possible indirect effects in modelling and a value of 0 is input to only use direct effects in modelling}
}
\details{this function must be run before using any other of the important functions in order to create initial input files} 
\value{Output is list of length 2: 
\item{AllNames}{list containing the names of the folders containing each imputed data set path analysis}
\item{AllDataNames}{list containing the name of the imputed datasets which is just length one element if there are no imputations. Also files are written and saved.}
}
\references{ \code{\link{MplusAutomation}} and \code{\link{mice}}}
\author{William Terry}
\note{the PasteIND=0 option should be used when the set of variables is too large to specify all indirect effects.  In selection, \code{\link{AllBackwardSelect}}, if the m plus first .inp file fails to run due to a singularity issue, consider creating a new InputInitializeMat.  See \code{\link{CreateInitializeMatrix}} for the format of the InputInitializeMat, and consider replacing some 1 values for included paths with 0s to not include the path.
It is also very important that all variables start with a capital letter and contain only numbers and letters (no spaces or special characters) in this version}
\seealso{\code{\link{CreateInitializeMatrix}}}
\examples{
\dontrun{

InitD=Simulate(MissingYN=1)
xxx=Initialize(InitD,NumImpute=3,WhichCat=c(1,1,1,1,0))

}
}
\keyword{data}
\keyword{Variable Selection}
\keyword{Impute}