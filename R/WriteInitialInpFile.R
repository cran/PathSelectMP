WriteInitialInpFile <-
function(AllData,InitialData,InputInitializeMat,IndList,Directry,NADes,WhichCat,WhichImpute,WhichRowsImp,AllMethods){

#All data is in the format c(NameFile,DataFileName,ImputeSeed)
NameFile=AllData[[1]]
DataFileName=AllData[[2]]
ImputeSeed=AllData[[3]]

TO=Directry
TO=paste(TO,"/",NameFile,sep="")
dir.create(TO)

N=names(InitialData)
if(sum(WhichCat==1)!=0){
IndepCount=apply(InputInitializeMat,1,sum)
NC=N[WhichCat==1 & IndepCount>0]
}else{
NC=c()
}

Writer=c()
Writer=paste("TITLE:","\n",sep="")
Writer=paste(Writer,"DATA:","\n","FILE IS ",DataFileName,".dat;","\n",sep="")

DataFileNameS=paste0(TO,"/",DataFileName,".dat")
if(length(ImputeSeed)!=0){
DatImputations(InitialData,ImputeSeed,NADes,DataFileNameS,WhichCat,WhichImpute,WhichRowsImp,AllMethods)
}else{
ID=seq(1,nrow(InitialData))
InitialData=cbind(InitialData,ID)
write.table(InitialData,DataFileNameS,sep="\t",row.names=FALSE,col.names=FALSE)
}

Writer=paste(Writer,"VARIABLE:","\n",sep="")
Writer=paste(Writer,"NAMES ARE ",paste0(paste(N,collapse=" ")," IDvar"),";","\n",sep="")
Writer=paste(Writer,"IDVARIABLE = IDvar;","\n",sep="")
Writer=paste(Writer,"MISSING ARE ",paste(N,collapse=" "),"(",as.character(NADes[1]),")",";","\n",sep="")
Writer=paste(Writer,"USEVARIABLES ARE ",paste(N,collapse=" "),";","\n",sep="")
if(length(NC)!=0){
Writer=paste(Writer,"CATEGORICAL ARE ",paste(NC,collapse=" "),";","\n",sep="")
}
Writer=paste(Writer,"ANALYSIS:","\n",sep="")
Writer=paste(Writer,"ESTIMATOR = ","WLSMV",";","\n",sep="")
Writer=paste(Writer,"PARAMETERIZATION = ","THETA",";","\n",sep="")
Writer=paste(Writer,"MODEL=","NOCOVARIANCES",";","\n",sep="")
Writer=paste(Writer,"MODEL:","\n",sep="")
Writer2=paste(sapply(c(1:length(N)),PathNames,InputInitializeMat),sep="")
Writer2=paste0(Writer2,collapse="")
Writer=paste0(Writer,Writer2)
Writer=paste0(Writer,"\n","MODEL INDIRECT:","\n","\n")
Writer2=paste(sapply(IndList,paste,collapse=" ",sep=""),collapse=";\n")
Writer=paste0(Writer,Writer2,";","\n")
Writer=paste0(Writer,"SAVEDATA:","\n")
Writer=paste0(Writer,"FILE IS ",paste0("MPlus",DataFileName,".txt"),";","\n")
i=1
NewFileName=paste(NameFile,"_",as.character(i),".INP",sep="")
writeLines(Writer,paste0(TO,"/",NewFileName),sep="\n")
return(Writer)}
