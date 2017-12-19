CreateSummaryMats <-
function(FileName,OutputSE=FALSE,OutputPVal=FALSE,Directry,OutputFinalMat=TRUE){

BeforeOnVar<-function(ONLines,DirectEffs){
STARTbo=gregexpr("[A-Z]",DirectEffs[ONLines])[[1]][1][1]
ENDbo=gregexpr("ON",DirectEffs[ONLines])[[1]][1][1]
FirstWSpace=substr(DirectEffs[ONLines],STARTbo,(ENDbo-1))
FirstSpaceLoc=gregexpr(" ",FirstWSpace)[[1]][1][1]
varName=substr(FirstWSpace,1,(FirstSpaceLoc-1))
return(varName)}

AfterOnVars<-function(ONLineseq,ONLinesNew,EmptyLines,DirShort){
VarsANDVals=DirShort[(ONLinesNew[ONLineseq]+1):(EmptyLines[ONLineseq]-1)]
return(VarsANDVals)}

TO=paste(Directry,"/",FileName,sep="")
files <- list.files(path=TO,pattern = ".out$")

hh=lapply(strsplit(files,"_"),NumEndFile,".out",'o')

hh=as.numeric(paste(hh))
LastFileName=files[which(hh==max(hh))]

f=paste(readLines(paste(TO,"/",LastFileName,sep="")),collapse="\n")
fs=strsplit(f,"\n")[[1]]
StartDir=match("MODEL RESULTS",fs)
EndDir=match("QUALITY OF NUMERICAL RESULTS",fs)
DirectEffs=fs[StartDir:EndDir]
ONLines=grep("ON",DirectEffs)

BeforeOnVars=lapply(ONLines,BeforeOnVar,DirectEffs)

LASTLine1=match("",DirectEffs[ONLines[length(ONLines)]:length(DirectEffs)])
LASTLine=ONLines[length(ONLines)]+LASTLine1-1
DirShort=DirectEffs[ONLines[1]:(LASTLine)]
ONLinesNew=grep("ON",DirShort)
EmptyLines=which(match(DirShort,"")==1)
ONLineseq=seq(1,length(ONLinesNew))


VarsANDVals=lapply(ONLineseq,AfterOnVars,ONLinesNew,EmptyLines,DirShort)

EachIndDirSpc<-function(VarsANDVals){
StartSpaces=lapply(VarsANDVals,gregexpr,pattern="  ")
return(StartSpaces)}

EachIndDirV<-function(VarsANDVals){
StartVarLoc=lapply(VarsANDVals,gregexpr,pattern="[A-Z]")
return(StartVarLoc)}

StartVarsLoc=lapply(VarsANDVals,EachIndDirV)
StartVarsLocS=lapply(VarsANDVals,EachIndDirSpc)
INDLbig=c()
INDLsmall=c()
EFFbig=c()
EFFsmall=c()
for(i in 1:length(StartVarsLoc)){
INDLsmall=c()
EFFsmall=c()
for(j in 1:length(StartVarsLoc[[i]])){
st=as.numeric(StartVarsLoc[[i]][[j]][[1]])
stp=as.numeric(StartVarsLocS[[i]][[j]][[1]])
VAR=substr(VarsANDVals[[i]][j],st,(stp[which(stp>st[length(st)])][1]-1))
AfterVarLoc=stp[which(stp>st[length(st)])][1]

AllVarsVals=VarsANDVals[[i]][j]
AllVals=substr(AllVarsVals,AfterVarLoc,nchar(AllVarsVals))

ValsVec=strsplit(AllVals," ")[[1]]
ValsVec=as.numeric(ValsVec[which(ValsVec!="")])

NUMfirst=ValsVec[1]
NUMSecond=ValsVec[2]
NUMfourth=ValsVec[4]

INDLsmall=c(INDLsmall,VAR)
if(OutputSE==TRUE){
EFFsmall=c(EFFsmall,NUMSecond)
}
if(OutputPVal==TRUE){
EFFsmall=c(EFFsmall,NUMfourth)
}
if(OutputPVal==FALSE & OutputSE==FALSE){
EFFsmall=c(EFFsmall,NUMfirst)
}
}
INDLbig=c(INDLbig,list(INDLsmall))
EFFbig=c(EFFbig,list(as.numeric(EFFsmall)))
}

USEVarsLoc=grep("USEVARIABLES ARE",fs)
USEsp=strsplit(fs[USEVarsLoc]," ")[[1]]
ARELoc=which(USEsp=="ARE")
VARlist=USEsp[(ARELoc+1):length(USEsp)]
SPACEQ=as.numeric(gregexpr(VARlist[length(VARlist)],pattern=" ")[[1]])
if(SPACEQ>0){
VARlist[length(VARlist)]=substr(VARlist[length(VARlist)],1,(SPACEQ-1))
}
SEMIQ=as.numeric(gregexpr(VARlist[length(VARlist)],pattern=";")[[1]])
if(SEMIQ>0){
VARlist[length(VARlist)]=substr(VARlist[length(VARlist)],1,(SEMIQ-1))
}
StoreMatEff=matrix(rep(0,length(VARlist)^2),nrow=length(VARlist))
StoreMatEff=data.frame(StoreMatEff)
names(StoreMatEff)=VARlist
row.names(StoreMatEff)=VARlist

for(i in 1:length(StartVarsLoc)){
BeforeOnRow=which(row.names(StoreMatEff)==BeforeOnVars[[i]][1])
for(j in 1:length(StartVarsLoc[[i]])){
AfterOnRow=which(names(StoreMatEff)==INDLbig[[i]][j])
StoreMatEff[BeforeOnRow,AfterOnRow]=EFFbig[[i]][j]
}
}
if(OutputFinalMat==FALSE){
return(c("Dependent"=list(c(unlist(BeforeOnVars))),"Independent"=list(INDLbig),"DirVals"=list(EFFbig)))}
if(OutputFinalMat==TRUE){
write.table(StoreMatEff,file=paste(TO,"/results.csv",sep=""),sep=",")
return(StoreMatEff)
}
}
