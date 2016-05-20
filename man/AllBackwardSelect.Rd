\name{AllBackwardSelect}
\alias{AllBackwardSelect}
\title{Run Backward Variable Selection in Path Analysis with M Plus}
\description{performs backward selection in M Plus for all input data sets which is either 1 data set or more if imputations specified}
\usage{AllBackwardSelect(AllNames, Directry = getwd(), PSig = 0.05)}
\arguments{
  \item{AllNames}{string name of folder containing .inp and .out M Plus files, and also start of filenames for .inp and .out files}
  \item{Directry}{the path containing the folder where the M Plus files are saved and run default is current working directory}
  \item{PSig}{the alpha value to use for accepting or rejecting null hypothesis that coefficient is equal to zero in path}
}
\details{Backward variable selection is performed for an entire path where 1 variable at a time is deleted where the variable which is deleted has the highest p value for a total effect which is greater than \code{PSig}, and if no such variable in a path with corresponding p-value exists then the variable in a path with the highest p value for an indirect effect which is greater than \code{PSig} is deleted, and finally if no such variable in a path with corresponding p-value exists for the first two cases, then the variable in a path with the highest p value for a direct effect which is greater than \code{PSig} is deleted.  This process repeats until no p-values for effects are greater than \code{PSig}.
}
\value{
There is no output from this function rather M Plus .inp and .out files are written and saved in the specified folder and path one at a time as backward stepwise variable selection is performed and updated.
}
\references{M Plus and \code{\link{MplusAutomation}}}
\author{William Terry}
\note{This is one of the most important functions for the user in this package.  Also make sure the folder where the .inp and .out files are saved does not exist yet or exists but is empty. Also if re-running with same folder, delete previous results before re-running. if the m plus first .inp file fails to run due to a singularity issue, consider creating a new InputInitializeMat for \code{\link{Initialize}}.  See \code{\link{CreateInitializeMatrix}} for the format of the InputInitializeMat, and consider replacing some 1 values for included paths with 0s to not include the path.}
\examples{
\dontrun{

InitD=Simulate(n=1000,MissingYN=1)
xxx=Initialize(InitD,NumImpute=3,WhichCat=c(1,1,1,1,0))
ggg=AllBackwardSelect(xxx[[1]])
}
}
\keyword{Variable Selection}
