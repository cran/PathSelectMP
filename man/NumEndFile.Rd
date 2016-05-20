\name{NumEndFile}
\alias{NumEndFile}
\title{Extract Number From INP and OUT Files}
\description{helper function used to find last input and output file which is the highest numbered file}
\usage{NumEndFile(NameoFile, pattern1, pattern2)}
\arguments{
  \item{NameoFile}{list of strings}
  \item{pattern1}{the string pattern before which the desired number is located}
  \item{pattern2}{2nd character of string pattern before which the desired number is located}
}
\details{helper function not to be used by regular user}
\value{returns number}
\references{No reference}
\author{William Terry}
\note{No notes}
\examples{
\dontshow{
NumEndFile<-function(NameoFile,pattern1,pattern2){
	#print(NameoFile[2])
	LocUse=grep(NameoFile,pattern=pattern1)
	Loc=gregexpr(pattern =pattern2,NameoFile[LocUse])[[1]][1]
	num=substr(NameoFile[LocUse],1,(Loc-2))
	#print(num)
	return(as.numeric(num))
	}
}
files=c("new_1.out","new_10.out","new_11.out","new_12.out")
hh=lapply(strsplit(files,"_"),NumEndFile,pattern1=".out",pattern2="o")
}

\keyword{Parse}
\keyword{helper}
