Initialize <-
function(InitialData,NumImpute=0,DataFileName="NewData",NameFile="New",Directry=getwd(),NADes=c(-99),
startSeedImputations=1000,InputInitializeMat="N",WhichCat=rep(1,ncol(InitialData)),AllMethods=c("logreg","polr","pmm"),
WhichImpute=rep(1,ncol(InitialData)),WhichRowsImp=c(1:nrow(InitialData)),PasteIND=1){

Check=CheckVarNames(InitialData)
if(Check=="CheckFailed"){
return()}
if(length(InputInitializeMat)==1){
if(length(ncol(InputInitializeMat[1]))==0){
if(InputInitializeMat=="N"){
InputInitializeMat=CreateInitializeMatrix(InitialData,WhichCat)
}
}
}
for(i in 1:ncol(InitialData)){
if(WhichCat[i]==0){
if(sum(InputInitializeMat[i,])!=0){
print("Continous Outcome Variables Not Permissible")
print("InputInitializeMat has been modified accordingly")
InputInitializeMat[i,]=rep(0,length(InputInitializeMat[i,]))
}
}
}
IndList=AddOnINDStatements(InputInitializeMat,PasteIND)
if(NumImpute!=0){
NumImpList=seq(startSeedImputations,(startSeedImputations+NumImpute-1),1)
NameFile=paste0(NameFile,"_")
DataFileName=paste0(DataFileName,"_")
AllData=ConvertData3(NameFile,DataFileName,NumImpList)
WRt=lapply(AllData,WriteInitialInpFile,InitialData,InputInitializeMat,IndList,Directry,NADes,WhichCat,WhichImpute,WhichRowsImp,AllMethods)
AllNames=sapply(AllData,OnlyNumberElement,1)
AllDataNames=sapply(AllData,OnlyNumberElement,2)
}else{
NumImpList=c()
AllData=c(list(NameFile),list(DataFileName),list(c()))
WRt=WriteInitialInpFile(AllData,InitialData,InputInitializeMat,IndList,Directry,NADes,WhichCat,WhichImpute,WhichRowsImp,AllMethods)
AllNames=NameFile
AllDataNames=DataFileName
}
return(c(list("AllNames"=AllNames),list("AllDataNames"=AllDataNames)))}
