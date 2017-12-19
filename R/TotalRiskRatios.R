TotalRiskRatios <-
function(AllDat,Directry=getwd(),InputDepVal=1){

AssignMatFun<-function(AllDat,InputDepVal){
LastFileOut=AllDat[[1]]
print(LastFileOut)
TO=paste(Directry,"/",LastFileOut,sep="")
print(TO)
files <- list.files(path=TO,pattern = ".out$")

hh=lapply(strsplit(files,"_"),NumEndFile,pattern1=".out",pattern2='out')
hh=as.numeric(paste(hh))
LastFileName=files[which(hh==max(hh))]

print(LastFileName)
x=scan(paste(TO,"/",LastFileName,sep=""),what=character())

GG<-ParseTotalEffects2(x)

RNAMES<-unique(GG[[2]][,3])
CNAMES<-unique(GG[[2]][,1])

MRname<-match(GG[[2]][,3],RNAMES)
LMRname<-as.numeric(sapply(split(MRname,MRname),length))

y=paste(readLines(paste(TO,"/",LastFileName,sep="")),collapse="\n")

ys=strsplit(y,"\n")[[1]]
ThreshLoc=grep("Thresholds",ys)
EndThresh=which(ys[ThreshLoc:length(ys)]=="")[1]+ThreshLoc-1
Thresh=ys[(ThreshLoc+1):(EndThresh-1)]

AllThresh=sapply(Thresh,strsplit," ")
DL=lapply(AllThresh,match,"")
AllThreshNoSp=SpecialMatch(AllThresh,DL)
ThreshMat=matrix(unlist(AllThreshNoSp),nrow=5)
ThresholdVals<-as.numeric(ThreshMat[2,])
ThresholdNames<-ThreshMat[1,]

MatchThresh<-(lapply(RNAMES,grep,ThresholdNames))
REM<-which(sapply(MatchThresh,length)==0)
if(length(REM)!=0){
MatchThreshKeep<-MatchThresh[-REM]
}else{
MatchThreshKeep<-MatchThresh
}
NAMErr=c()
StoreRR=c()
LMRcount=0

for(i in 1:length(MatchThreshKeep)){
for(k in 1:LMRname[i]){
LMRcount=LMRcount+1
TotEffVal=GG[[1]][LMRcount]
if(abs(TotEffVal)>0.001){
for(m in length(ThresholdVals[MatchThreshKeep[[i]]]):1){
if(m==length(ThresholdVals[MatchThreshKeep[[i]]])){
StoreWThresh1=-ThresholdVals[MatchThreshKeep[[i]][m]]+TotEffVal*InputDepVal
StoreWThreshF1=pnorm(StoreWThresh1)
StoreWThreshSubtr1=StoreWThreshF1
RRatio=StoreWThreshSubtr1/(pnorm(-ThresholdVals[MatchThreshKeep[[i]][m]]))
if(is.matrix(RRatio)==FALSE){
RRatio=matrix(RRatio,nrow=1)
}
}
if(m<length(ThresholdVals[MatchThreshKeep[[i]]])){
StoreWThresh1=ThresholdVals[MatchThreshKeep[[i]][m]]-TotEffVal*InputDepVal
StoreWThresh2=ThresholdVals[MatchThreshKeep[[i]][m+1]]-TotEffVal*InputDepVal
StoreWThreshF1=pnorm(StoreWThresh1)
StoreWThreshF2=pnorm(StoreWThresh2)
StoreWThreshSubtr1=StoreWThreshF2-StoreWThreshF1
RRatio=StoreWThreshSubtr1/(pnorm(ThresholdVals[MatchThreshKeep[[i]][m+1]])-pnorm(ThresholdVals[MatchThreshKeep[[i]][m]]))
if(is.matrix(RRatio)==FALSE){
RRatio=matrix(RRatio,nrow=1)
}
}
StoreRR=c(StoreRR,list(RRatio))
NAMErr=c(NAMErr,list(paste(ThresholdNames[MatchThreshKeep[[i]]][m],GG[[2]][,1][LMRcount])))
}
}
}
}
names(StoreRR)=NAMErr
AllNames=strsplit(names(StoreRR)," ")
AllThresh=unique(unlist(AllNames)[mod(seq(1,length(unlist(AllNames))),2)==1])
AllInDep=unique(unlist(AllNames)[mod(seq(1,length(unlist(AllNames))),2)==0])
RRresults=matrix(rep(0,length(AllThresh)*length(AllInDep)),nrow=length(AllThresh))
RRresults=data.frame(RRresults)
row.names(RRresults)=AllThresh
names(RRresults)=AllInDep
for(p in 1:length(StoreRR)){
FND=AllNames[[p]]
row=which(row.names(RRresults)==FND[1])
col=which(names(RRresults)==FND[2])
for(t in 1:length(col)){
RRresults[row,col[t]]=StoreRR[[p]][t]
}
}
return(RRresults)}

RRresults=AssignMatFun(AllDat,InputDepVal)
return(RRresults)}
