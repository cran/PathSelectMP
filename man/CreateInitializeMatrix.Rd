\name{CreateInitializeMatrix}
\alias{CreateInitializeMatrix}
\title{Create Possible Path Matrix}
\description{creates a dataframe with 1's and 0s' where a 1 represents inclusion in the path and 0 represents exlusion}
\usage{
CreateInitializeMatrix(InitialData, WhichCat, empty = FALSE)
}
\arguments{
  \item{InitialData}{the dataframe with the initial data}
  \item{WhichCat}{a vector of 0's and 1's length of number of columns of \code{InitialData} where 1 is a categorical variable and 0 is a continuous variable}
  \item{empty}{if \code{empty} is TRUE then a dataframe of all 0's is returned to allow user to individually assign 1's for path inclusion more easily}
}
\details{
the rows represent dependent variables in the path and the columns represent independent variables in the paths with a 1 representing in M Plus terminology row variable ON column variable
}
\value{
returns matrix required by \code{\link{Initialize}} for all initial paths to run the default is an upper triangular matrix. This is a square matrix and names of rows is the same  as the names of columns}
\references{M Plus}
\author{William Terry}
\note{In this version only Categorical variables can be dependent variables and if using the default upper triangular matrix, due to the current way indirect effects are created, only approximately 8 or 9 variables can be used with reasonable computing time.  The input to \code{\link{Initialize}} must be in the same format as this output so user may want to use this function with  \code{empty} as FALSE and subtitute 1's for desired possible paths.
Also note that row names and column names must be the same and must start with a capital letter and only contains numbers and letters in this version.}
\examples{
\dontshow{
Simulate<-function(n=1000,seedNum=1000,MissingYN=0,exampleNum=1){
	if(exampleNum==1){	
		set.seed(seed=seedNum)	
		#example 1
		W=rbinom(n,1,0.55)
		Y=rnorm(n)
		Z=rbinom(n,1,0.40)
	
		V1=2.5*Z
		V2=(1/(1+exp(-V1)))
		V3=rbinom(n,1,V2)

		X1=2.5*V3+W-0.5*Z
		X2=(1/(1+exp(-X1)))
		X3=rbinom(n,1,X2)

		NewData=data.frame(X3,V3,W,Z,Y)
		names(NewData)=c("X","V","W","Z","Y")
		}

	if(exampleNum==2){
		#example 2
		set.seed(seed=seedNum)	
		A=rbinom(n,1,0.55)
		B=rbinom(n,3,0.50)
		C=rnorm(n)
		E=rbinom(n,5,0.25)
		F=rnorm(n)
		W=rnorm(n)

		G=1.5*C+E+3.5
		G1=(1/(1+exp(-G)))
		G2=rbinom(n,3,G1)

		H=2.5*B+2*C+1-2*rnorm(n,1,2)
		H1=(1/(1+exp(-H)))
		H2=rbinom(n,2,H1)

		J=3*H-1+B+C
		J1=(1/(1+exp(-J)))
		J2=rbinom(n,2,J1)

		NewData=data.frame(J2,H2,G2,A,B,C,E,F,W)
		names(NewData)=c("J","H","G","A","B","C","E","F","W")
		}

	if(exampleNum==3){
		#example 3
		set.seed(seed=seedNum)	
		A=rbinom(n,1,0.55)+1
		B=rbinom(n,3,0.50)+1
		C=rnorm(n)+1
		E=rbinom(n,5,0.25)+1
		F=rnorm(n)+1

		G=1.5*A+E-4*C+2*F
		G1=(1/(1+exp(-G)))
		G2=rbinom(n,3,G1)

		H=2.5*B+2*C+1-2*rnorm(n,1,2)
		H1=(1/(1+exp(-H)))
		H2=rbinom(n,2,H1)

		J=3*H-1
		J1=(1/(1+exp(-J)))
		J2=rbinom(n,2,J1)

		NewData=data.frame(J2,H2,G2,A,B,C,E,F)
		names(NewData)=c("J","H","G","A","B","C","E","F")
		}


	if(MissingYN==1){
		for(i in 1:ncol(NewData)){
			Ch=sample(1:n,n*.1)
			NewData[Ch,i]=(-99)
			}		
		}
	return(NewData)}

CreateInitializeMatrix<-function(InitialData,WhichCat,empty=FALSE){
	N=names(InitialData)
	InitMat=matrix(rep(0,length(N)*length(N)),nrow=length(N))
	InitDatF=data.frame(InitMat)
	names(InitDatF)=N
	row.names(InitDatF)=N
	if(empty==TRUE){
		return(InitDatF)
	}else{
		InitMat[upper.tri(InitMat)]=1
		InitMat[WhichCat!=1,]=0
		InitDatF=data.frame(InitMat)
		names(InitDatF)=N
		row.names(InitDatF)=N
		return(InitDatF)
		}
	}

}

InitD=Simulate()
I=CreateInitializeMatrix(InitD,WhichCat=c(rep(1,ncol(InitD)-1),0))

}
\keyword{Initialize}


