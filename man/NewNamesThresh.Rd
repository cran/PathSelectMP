\name{NewNamesThresh}
\alias{NewNamesThresh}
\title{Threshold Names}
\description{Converts M Plus threshold names to the name of the category given in the data}
\usage{
NewNamesThresh(FileName, DataName, ThreshName, 
InitialData, Directry = getwd(), NADes = c(-99))
}
\arguments{
  \item{FileName}{name of .out Mplus file}
  \item{DataName}{name of .inp Mplus file}
  \item{ThreshName}{string threshold name given by M Plus}
  \item{InitialData}{dataframe used}
  \item{Directry}{the working directory which contains the folder which contains .inp and .out files}
  \item{NADes}{the value for missing data}
}
\details{threshold names given by M Plus and consequently many output matrices in this package are designated starting with 1 and ordered, and this function converts 1 or other designation to the actual category in the data}
\value{returns string of variable value which is used for the threshold}
\references{M Plus}
\author{William Terry}
\note{make sure working directory is set to location of folder containing folder which contains .out m plus files}
\examples{
\dontrun{
InitD=Simulate(MissingYN=1)
xxx=Initialize(InitD,NumImpute=3,WhichCat=c(1,1,1,1,0))
ggg=AllBackwardSelect(xxx[[1]])
yyy=AllSummary2(xxx[[1]])
zzz=AllSummary(xxx[[1]])[[2]]
qqq=AddOnAllInd(xxx[[1]],zzz)
NewNamesThresh("New_1000","NewData_1000","X$1",InitD)
}
}
\keyword{Threshold}
\keyword{data}

