CreateTotalEffMat <-
function(FileName,Directry){
LastFileOut=FileName
print(LastFileOut)
TO=paste0(Directry,"/",LastFileOut)
files <- list.files(path=TO,pattern = ".out$")

hh=lapply(strsplit(files,"_"),NumEndFile,pattern1=".out",pattern2="out")
hh=as.numeric(paste(hh))
LastFileName=files[which(hh==max(hh))]

print(LastFileName)
x=scan(paste(TO,"/",LastFileName,sep=""),what=character())

GG=ParseTotalEffects2(x,PVal=FALSE)
GGp=ParseTotalEffects2(x,PVal=TRUE)
GGse=ParseTotalEffects2(x,StandardError=TRUE)

y=paste(readLines(paste(TO,"/",LastFileName,sep="")),collapse="\n")
ys=strsplit(y,"\n")[[1]]

NAMESLoc=grep("USEVARIABLES ARE",ys)
NAMES=ys[NAMESLoc]
Nvec=strsplit(NAMES," ")[[1]]
if(substr(Nvec[length(Nvec)],nchar(Nvec[length(Nvec)]),nchar(Nvec[length(Nvec)]))==";"){
Nvec[length(Nvec)]=substr(Nvec[length(Nvec)],1,nchar(Nvec[length(Nvec)])-1)
}
Nvec=Nvec[(which(Nvec=="ARE")+1):length(Nvec)]

TotalEffMat=matrix(rep(0,length(Nvec)*length(Nvec)),nrow=length(Nvec))
TotalEffMat=data.frame(TotalEffMat)
names(TotalEffMat)=Nvec
row.names(TotalEffMat)=Nvec

TotalPValMat=matrix(rep(0,length(Nvec)*length(Nvec)),nrow=length(Nvec))
TotalPValMat=data.frame(TotalPValMat)
names(TotalPValMat)=Nvec
row.names(TotalPValMat)=Nvec

TotalSEMat=matrix(rep(0,length(Nvec)*length(Nvec)),nrow=length(Nvec))
TotalSEMat=data.frame(TotalSEMat)
names(TotalSEMat)=Nvec
row.names(TotalSEMat)=Nvec

TotalCount=matrix(rep(0,length(Nvec)*length(Nvec)),nrow=length(Nvec))
TotalCount=data.frame(TotalCount)
names(TotalCount)=Nvec
row.names(TotalCount)=Nvec

q1=match(GG[[2]][,3],Nvec)
q2=match(GG[[2]][,1],Nvec)
EffVals=GG[[1]]
PVals=GGp[[1]]
SEVals=GGse[[1]]
for(i in 1:length(EffVals)){
TotalEffMat[q1[i],q2[i]]=EffVals[i]
TotalSEMat[q1[i],q2[i]]=SEVals[i]
TotalPValMat[q1[i],q2[i]]=PVals[i]
TotalCount[q1[i],q2[i]]=1
}

return(c("TotalEffects"=list(TotalEffMat),"TotalEffectsStandardError"=list(TotalSEMat),"TotalEffectsPVals"=list(TotalPValMat),"TotalEffectsCount"=list(TotalCount)))}
