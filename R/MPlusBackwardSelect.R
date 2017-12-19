MPlusBackwardSelect <-
function(FileName,Directry,PSig){
print(FileName)
OriginalFileName=FileName
Folder=paste(Directry,"/",FileName,"/",sep="")
FileName=paste(FileName,"_",sep="")

Rtrn1stElement<-function(lstStrings){
return(lstStrings[1])}

MAX<-function(ListNums,ERRORNum){
ListNums2=(order(ListNums,decreasing=TRUE))
RetV=ListNums[ListNums2[ERRORNum+1]]
start=ERRORNum+1
#FIX ERROR P-VALUE 1.00 WHEN NO INDIRECT EFFECT
while(RetV>0.9999){
start=start+1
RetV=ListNums[ListNums2[start]]
}
return(RetV)}

i=1
y=paste(readLines(paste(Folder,FileName,as.character(i),".INP",sep="")),collapse="\n")
ys=strsplit(y,"\n")[[1]]

TotGreaterPSigYN=1
ErrorCount=0
while(TotGreaterPSigYN>0){

runModels(Folder,logFile="MH_RunLog.txt",showOutput=FALSE,replaceOutfile="never")
x=scan(paste(Folder,FileName,as.character(i),".out",sep=""),what=character())
v1=paste(x,collapse=" ")
ErrorQ=gregexpr("THE STANDARD ERRORS OF THE MODEL PARAMETER ESTIMATES COULD NOT BE COMPUTED",v1)[[1]][1]
DirectDelete=0
IndirectDelete=0
if(ErrorQ>0){
#Error handler that has remove next best p-value variable when error removing best
ErrorCount=ErrorCount+1
print("ERROR")
print("THE STANDARD ERRORS OF THE MODEL PARAMETER ESTIMATES COULD NOT BE COMPUTED")
files <- list.files(path=Folder,pattern = ".out$")
hh=lapply(strsplit(files,"_"),NumEndFile,pattern1=".out",pattern2="o")
hh=as.numeric(paste(hh))
LastFileName=files[which(hh==max(hh))]
file.remove(paste0(Folder,"/",LastFileName))
OutLoc=gregexpr(pattern ='.out',LastFileName)[[1]][1]
NewLast=paste(substr(LastFileName,1,OutLoc-1),".INP",sep="")
file.remove(paste0(Folder,"/",NewLast))
i=i-1
if(i==0){
print("THE INITIAL MODEL FAILED TO RUN IN MPLUS")
print("TRY A DIFFERENT INITIALIZE MATRIX WITH POSSIBLE PREDICTOR COMBINATIONS")
print("OR JUST TRY A DIFFERENT MODEL")
return()
}
x=scan(paste(Folder,FileName,as.character(i),".out",sep=""),what=character())
}else{
ErrorCount=0
}
gg=ParseTotalEffects(x,OriginalFileName,Directry)
M=MAX(gg[[1]],ErrorCount)
if(M>=PSig){
DesName=gg[[3]][which(gg[[1]]==M)[1],]
print(DesName)
IndirectDelete=1
DirectDelete=1
}
if(M<PSig){
M=MAX(gg[[2]],ErrorCount)
if(M>=PSig){
DesName=gg[[3]][which(gg[[2]]==M)[1],]
print(DesName)
IndirectDelete=1
}
if(M<PSig){
M=MAX(gg[[4]],ErrorCount)
if(M>=PSig){
DesName=gg[[5]][which(gg[[4]]==M)[1],]
print(DesName)
DirectDelete=1
}else{
TotGreaterPSignYN=0
break
 }
}
}
#in the case of a tie for p-value just use 1st one in matrix
if(is.matrix(DesName)==TRUE){
DesName=DesName[1,]
}

y=paste(readLines(paste(Folder,FileName,as.character(i),".INP",sep="")),collapse="\n")
ys=strsplit(y,"\n")[[1]]

Categorical=grep("CATEGORICAL",ys)
StartModel=which(ys=="MODEL:")
StartModelInd=which(ys=="MODEL INDIRECT:")
DeletedSemi=0

#For MODEL part
WHOLE=ys[StartModel:StartModelInd]
Spl=strsplit(WHOLE," ")
Loc1st=which(as.numeric(mapply(match,DesName[[3]],Spl))==1)
StrW1st=ys[StartModel:StartModelInd][Loc1st]
Spl2nd=strsplit(StrW1st," ")[[1]]
Loc2nd=which(match(Spl2nd,DesName[[1]])==1)
if(length(Loc2nd)==0){
Loc2nd=which(match(Spl2nd,paste0(DesName[[1]],";"))==1)
DeletedSemi=1
}
SemiLoc=which(Spl2nd==";")
SemiSpLoc=which(Spl2nd==" ;")
Spl2ndNoSemi=Spl2nd
if(length(SemiLoc!=0) | length(SemiSpLoc)!=0){
if(length(SemiLoc!=0)){
Spl2ndNoSemi=Spl2ndNoSemi[-SemiLoc]}
if(length(SemiSpLoc!=0)){
Spl2ndNoSemi=Spl2ndNoSemi[-SemiSpLoc]}
}

WHOLEnew=WHOLE
ChangeCat=0
#this if clause in case only an IND effect and no Direct effect
if(length(Loc2nd!=0) & DirectDelete==1){
#print("DIRECT EFFECT DELETED")
#if clause in case it is the only variable in relationship
if(Loc2nd!=3 | (Loc2nd==3 & length(Spl2ndNoSemi)>3) ){
Spl2ndNew=Spl2nd[-Loc2nd]
if(DeletedSemi==1){
Spl2ndNew=c(Spl2ndNew,";")
}
StrW1stNew=paste(Spl2ndNew,collapse=" ")
WHOLEnew[Loc1st]=StrW1stNew
}else{
DeleteEntry=WHOLEnew[Loc1st]
WHOLEnew=WHOLEnew[-Loc1st]
#account for Categorical Statement
CategoricalList=strsplit(ys[Categorical]," ")
CategoricalListCl=CategoricalList[[1]][3:length(CategoricalList[[1]])]
LastE=CategoricalListCl[length(CategoricalListCl)]
if(substr(LastE,(nchar(LastE)),(nchar(LastE)))==";"){
CategoricalListCl[length(CategoricalListCl)]=substr(LastE,1,(nchar(LastE)-1))
}
DeleteCat=which(is.na(match(CategoricalListCl,DesName[3]))==FALSE)
if(length(DeleteCat)!=0){
CategoricalListNewCl=CategoricalListCl
CategoricalListNewCl=CategoricalListNewCl[-DeleteCat]
LastE=CategoricalListNewCl[length(CategoricalListNewCl)]
if(substr(LastE,(nchar(LastE)),(nchar(LastE)))!=";"){
CategoricalListNewCl[length(CategoricalListNewCl)]=paste(LastE,";",sep="")
}
CategoricalListNew=paste(c("CATEGORICAL ARE",CategoricalListNewCl),collapse=" ")
ChangeCat=1
}
}
}
#For MODEL INDIRECT PART note all indirect must follow var1 IND var2; format
WHOLEInd=ys[StartModelInd:length(ys)]
SplI=strsplit(WHOLEInd," ")
if(length(mapply(match,DesName[[3]],SplI))!=0){
LocI1st=which(as.numeric(mapply(match,DesName[[3]],SplI))==1)
StrWI1st=WHOLEInd[LocI1st]
SplI2nd=strsplit(StrWI1st," ")
LocI2nd=as.numeric(which(mapply(match,sapply(SplI2nd,paste),paste0(DesName[[1]],";"))==1))/3

WHOLEIndnew=WHOLEInd
if(IndirectDelete==1){
WHOLEIndnew=WHOLEIndnew[-(LocI1st[LocI2nd])]
}
if(DirectDelete==1 & IndirectDelete==0){
ChckDelI=(ParseTotalEffects2(x,Indirect=TRUE)[[1]])[(LocI1st[LocI2nd])-2]
#subtract 2 for blank space and Model Indirect statement

#check for numeric(0) so looks weird in if statement
if(length(ChckDelI==0)){
print("if triggered")
print(WHOLEIndnew[LocI1st[LocI2nd]])
WHOLEIndnew=WHOLEIndnew[-(LocI1st[LocI2nd])]
}
}

AllWHOLEnew=c(ys[1:(StartModel-1)],WHOLEnew[1:(length(WHOLEnew)-1)],WHOLEIndnew)
}else{
WHOLEIndnew=WHOLEInd
AllWHOLEnew=c(ys[1:(StartModel-1)],WHOLEnew[1:(length(WHOLEnew)-1)],WHOLEIndnew)
}

if(ChangeCat==1){
AllWHOLEnew[9]=CategoricalListNew
}

WNEWScript=paste(AllWHOLEnew,collapse="\n")
i=i+1
writeLines(WNEWScript,paste(Folder,FileName,as.character(i),".INP",sep=""))
}
}
