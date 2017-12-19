CalculatRiskRatios <-
function(AllDat,NADes,Directry=getwd(),WhichCat,WhichRiskCalc){
#WhichRiskCalc is a vector same length as WhichCat which specifies the value used for each variable in risk ratio calculations
#if value is zero then the default is used which is all possible values for categorical (separate risk ratio for each and compared to zero)
#and average of all continuous values for continuous variable compared to zero with 
#user can specify another value or a list of values for each variable if a list rather than one value is used then averaging over these values is performed
#and a risk ratio is given for each value

LastFileOut=AllDat[[1]]
print(LastFileOut)
TO=paste(Directry,"/",LastFileOut,sep="")

files <- list.files(path=TO,pattern = ".out$")

hh=lapply(strsplit(files,"_"),NumEndFile,pattern1=".out",pattern2='out')
hh=as.numeric(paste(hh))
LastFileName=files[which(hh==max(hh))]

x=AllDat[[3]]
y=paste(readLines(paste(TO,"/",LastFileName,sep="")),collapse="\n")

NonZeroIndDep<-function(MatchNZ,DirEffMat){
IndIces=which(abs(DirEffMat[MatchNZ,])>0.01)
return(IndIces)}

NonZeroIndDepVals<-function(MatchNZ,DirEffMat){
Vals=DirEffMat[MatchNZ,][which(abs(DirEffMat[MatchNZ,])>0.01)]
return(Vals)}

NewNames<-function(NumDep,x){
RetV=names(x)[NumDep]
return(RetV)}

SpecialAppend<-function(ListO,posFixOne,AppendVal){
ListwO=append(ListO,c(),posFixOne)
return(ListwO)}

SpecialMult<-function(x,z){
q=sum(x*z)
return(q)}
DeleteElement<-function(ListO,List1){
if(length(which(ListO==1))!=0){
List1=List1[-which(ListO==1)]
}else{
List1=List1}
return(List1)}

SpecialDiv<-function(ListO){
RetV=(ListO[1:(length(ListO)-1)])/(ListO[length(ListO)])
return(RetV)}

DatName=paste0(TO,"/",AllDat[[2]],".DAT")
FROMFile=DatName

print(FROMFile)
rd=read.table(FROMFile,sep="\t",header=FALSE)
#account for id variables
rd=rd[,-ncol(rd)]

WDum=lapply(sapply(seq(1,ncol(rd)),list),NumCat,rd,NADes)

WDum=as.numeric(paste(WDum))

ys=strsplit(y,"\n")[[1]]

NAMESLoc=grep("NAMES ARE",ys)
NAMES=ys[NAMESLoc]
Nvec=strsplit(NAMES," ")[[1]]
if(substr(Nvec[length(Nvec)],nchar(Nvec[length(Nvec)]),nchar(Nvec[length(Nvec)]))==";"){
Nvec[length(Nvec)]=substr(Nvec[length(Nvec)],1,nchar(Nvec[length(Nvec)])-1)
}
Nvec=Nvec[(which(Nvec=="ARE")+1):length(Nvec)]

ThreshLoc=grep("Thresholds",ys)
EndThresh=which(ys[ThreshLoc:length(ys)]=="")[1]+ThreshLoc-1
Thresh=ys[(ThreshLoc+1):(EndThresh-1)]

AllThresh=sapply(Thresh,strsplit," ")
DL=lapply(AllThresh,match,"")
AllThreshNoSp=SpecialMatch(AllThresh,DL)
ThreshMat=matrix(unlist(AllThreshNoSp),nrow=5)
ThresholdVals=as.numeric(ThreshMat[2,])
ThresholdNames=ThreshMat[1,]

MatchThresh=(sapply(names(x),grep,ThresholdNames))
REM=which(sapply(MatchThresh,length)==0)
if(length(REM)!=0){
MatchThreshKeep=MatchThresh[-REM]
}else{
MatchThreshKeep=MatchThresh
}

MatchMatNZ=sapply(names(MatchThreshKeep),grep,row.names(x))
NumDep=sapply(MatchMatNZ,NonZeroIndDep,x)
InDepNames=sapply(NumDep,NewNames,x)
DepVals=sapply(MatchMatNZ,NonZeroIndDepVals,x)

QL=c()

StoreRR=c()
NAMErr=c()
RRMat=x

for(i in 1:length(NumDep)){
NumDums=WDum[which(is.na(match(Nvec,InDepNames[[i]]))==FALSE)]
MTCHnum=which(is.na(match(Nvec,InDepNames[[i]]))==FALSE)

if(length(MTCHnum)>1){
PossibleCo=apply(rd[,MTCHnum],2,unique)
}else{
PossibleCo=list(unique(rd[,MTCHnum]))
}
if(is.matrix(PossibleCo)==TRUE){
PossibleCo=lapply(apply(PossibleCo,2,list),OnlyNumberElement,1)
}

PossibleCo=mapply(DeleteElement,lapply(PossibleCo,match,NADes),PossibleCo)
if(is.matrix(PossibleCo)==TRUE){
PossibleCo=lapply(apply(PossibleCo,2,list),OnlyNumberElement,1)
}

PossibleCo1=c()
for(p in 1:length(MTCHnum)){
if(length(WhichRiskCalc[[MTCHnum[p]]])==1){
if(WhichRiskCalc[[MTCHnum[p]]]==0 & WhichCat[[MTCHnum[p]]]==1){
PossibleCo1=c(PossibleCo1,list(PossibleCo[[p]]))
}
if(WhichRiskCalc[[MTCHnum[p]]]==0 & WhichCat[[MTCHnum[p]]]==0){
L1=DeleteElement(match((rd[,MTCHnum[p]]),NADes),(rd[,MTCHnum[p]]))
 PossibleCo1=c(PossibleCo1,list(c(0,mean(L1))))
}
}
if(length(WhichRiskCalc[[MTCHnum[p]]])!=1){
PossibleCo1=c(PossibleCo1,list(WhichRiskCalc[[MTCHnum[p]]]))
}
if(length(WhichRiskCalc[[MTCHnum[p]]])==1){
if(WhichRiskCalc[[MTCHnum[p]]]!=0){
PossibleCo1=c(PossibleCo1,list(WhichRiskCalc[[MTCHnum[p]]]))
}
}
}
PossibleCo=PossibleCo1
PossibleCoC=PossibleCo
PivotVarNum=length(NumDep[[i]])
UseDpVls=(as.numeric(DepVals[[i]]))
h=c(1:length(InDepNames[[i]]))
for(j in 1:length(NumDep[[i]])){
n=length(NumDep[[i]])

PossibleCo=PossibleCoC
PossibleCo=rev(PossibleCo)

PossibleCoC=c(list(PossibleCoC[[length(PossibleCoC)]]),PossibleCoC)
PossibleCoC=PossibleCoC[-length(PossibleCoC)]

V=NewBinseqWrap(n=n,PossibleCoefs=PossibleCo)
RR=c()
Vnew0=sapply(V,SpecialAppend,j,0)

if(is.matrix(Vnew0)==FALSE){
Vnew0=matrix(Vnew0,nrow=1)
}
AllPivUn=unique(Vnew0[1,])
AllPivUn=sort(AllPivUn)
StoreProd=c()
for(k in 1:length(AllPivUn)){
Selected=Vnew0[,which(Vnew0[1,]==AllPivUn[k])]
if(is.matrix(Selected)==FALSE){
Selected=matrix(Selected,nrow=1)
}
StoreProd1MT=apply(Selected,2,SpecialMult,UseDpVls)
StoreProd=c(StoreProd,list(StoreProd1MT))
}

UseDpVlsFrontStore=UseDpVls[length(UseDpVls)]
UseDpVls=UseDpVls[-length(UseDpVls)]
UseDpVls=c(UseDpVlsFrontStore,UseDpVls)

PivotVarNum=PivotVarNum-1

StoreWThreshF=c()
StoreWThreshSubtr=c()
for(m in length(ThresholdVals[MatchThreshKeep[[i]]]):1){
if(m==length(ThresholdVals[MatchThreshKeep[[i]]])){
StoreWThresh1=lapply(StoreProd,"+",-1*ThresholdVals[MatchThreshKeep[[i]]][m])
StoreWThreshF1=lapply(StoreWThresh1,pnorm)
StoreWThreshF1=matrix(unlist(StoreWThreshF1),nrow=length(StoreWThreshF1),byrow=TRUE)
StoreWThreshSubtr1=StoreWThreshF1
RRatioA=apply(StoreWThreshSubtr1,1,mean)
}
if(m<length(ThresholdVals[MatchThreshKeep[[i]]])){
StoreWThresh1=lapply(StoreProd,"+",-1*ThresholdVals[MatchThreshKeep[[i]]][m])
StoreWThresh2=lapply(StoreProd,"+",-1*ThresholdVals[MatchThreshKeep[[i]]][m+1])
StoreWThreshF1=lapply(StoreWThresh1,pnorm)
StoreWThreshF2=lapply(StoreWThresh2,pnorm)
StoreWThreshF1=1-matrix(unlist(StoreWThreshF1),nrow=length(StoreWThreshF1),byrow=TRUE)
StoreWThreshF2=1-matrix(unlist(StoreWThreshF2),nrow=length(StoreWThreshF2),byrow=TRUE)
StoreWThreshSubtr1=StoreWThreshF2-StoreWThreshF1
RRatioA=apply(StoreWThreshSubtr1,1,mean)
}
RRatioNew=c()
for(b in 1:(length(RRatioA)-1)){
RRatioNew=c(RRatioNew,RRatioA[b+1]/RRatioA[1])
}
RRatioA=RRatioNew
StoreRR=c(StoreRR,list(RRatioA))
NAMErr=c(NAMErr,list(paste(ThresholdNames[MatchThreshKeep[[i]][m]],InDepNames[[i]][h[1]])))
}
h=c(h[length(h)],h)
h=h[-length(h)]
}
}
names(StoreRR)=NAMErr
AllNames<-strsplit(names(StoreRR)," ")
AllThresh<-unique(unlist(AllNames)[mod(seq(1,length(unlist(AllNames))),2)==1])
AllInDep<-unique(unlist(AllNames)[mod(seq(1,length(unlist(AllNames))),2)==0])
M<-match(AllInDep,Nvec)

Mdefault=which(is.na(match(WhichRiskCalc,0))==FALSE)
 Mloc=match(M,Mdefault)
Mloc2=which(is.na(Mloc)==TRUE)
AddM=M[Mloc2]

MM<-c()
if(length(AddM)!=0){
for(u in 1:length(M)){
if(is.na(match(M[u],AddM))==FALSE){
MM=c(MM,length(WhichRiskCalc[[M[u]]])-1)
}else{
if(WhichCat[M[u]]==1){
MM=c(MM,WDum[M[u]]-1)
}
if(WhichCat[M[u]]==0){
MM=c(MM,1)
}
}
}
MMM<-AllInDep[which(MM>1)]
}else{
MM<-WDum[M]-1
MMM<-AllInDep[which(MM>1)]
}
AFTERadd=0
AllInDep3=AllInDep
AllInDep2=AllInDep
if(length(MMM)!=0){
for(p in 1:length(MMM)){

Index1=M[which(MM>1)][p]
L=rep(MMM[p],MM[which(MM>1)][p]-1)
if(length(WhichRiskCalc[[Index1]])!=1){
L1=WhichRiskCalc[[Index1]][2:length(WhichRiskCalc[[Index1]])]
L2=lapply(unique(L),paste0,L1)[[1]]
AllInDep3=append(AllInDep3,L,after=which(MM>1)[p]+AFTERadd)
AllInDep2=append(AllInDep2,L2,after=which(MM>1)[p]+AFTERadd)
AllInDep2=AllInDep2[-(which(MM>1)[p]+AFTERadd)]
AFTERadd=AFTERadd+MM[which(MM>1)][p]-1
}
if(length(WhichRiskCalc[[Index1]])==1){
if(WhichRiskCalc[[Index1]]!=0){
print("Error:You need to specify atleast two values for a risk ratio to be calculated")
return()
}
if(WhichRiskCalc[[Index1]]==0){
if(WhichCat[[Index1]]==0){
L1=DeleteElement(match((rd[,Index1]),NADes),(rd[,Index1]))
L1=c(mean(L1))
L2=lapply(unique(L),paste0,L1)[[1]]
AllInDep2=append(AllInDep2,L2,after=which(MM>1)[p]+AFTERadd)
AllInDep2=AllInDep2[-(which(MM>1)[p]+AFTERadd)]

}else{
L1=DeleteElement(match(unique(rd[,Index1]),NADes),unique(rd[,Index1]))
L1=sort(L1)[2:length(L1)]
L2=lapply(unique(L),paste0,L1)[[1]]
AllInDep3=append(AllInDep3,L,after=which(MM>1)[p]+AFTERadd)
AllInDep2=append(AllInDep2,L2,after=which(MM>1)[p]+AFTERadd)
AllInDep2=AllInDep2[-(which(MM>1)[p]+AFTERadd)]
AFTERadd=AFTERadd+MM[which(MM>1)][p]-1
}
}
}
}
}
AllInDep=AllInDep2
RRresults=matrix(rep(0,length(AllThresh)*length(AllInDep)),nrow=length(AllThresh))
RRresults=data.frame(RRresults)
row.names(RRresults)=AllThresh
names(RRresults)=AllInDep
for(p in 1:length(StoreRR)){
FND=AllNames[[p]]
row=which(row.names(RRresults)==FND[1])
col=which(AllInDep3==FND[2])
for(t in 1:length(col)){
RRresults[row,col[t]]=as.numeric(StoreRR[[p]][t])
}
}
return(RRresults)
}
