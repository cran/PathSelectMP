CreateInitializeMatrix <-
function(InitialData,WhichCat,empty=FALSE){
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
