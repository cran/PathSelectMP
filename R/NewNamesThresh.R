NewNamesThresh <-
function(FileName,DataName,ThreshName,InitialData,Directry=getwd(),NADes=c(-99)){

SpaceRowRead<-function(LineNum,BigFile){
lineWspcs=paste(BigFile[LineNum,],collapse=" ")
lineWspcs1=strsplit(lineWspcs," ")
lineWspcs1=lineWspcs1[[1]][which(is.na(match(lineWspcs1[[1]],""))==TRUE)]
return(lineWspcs1)
}

StrThresh=strsplit(ThreshName,"")[[1]]
LocDlrSgn=which(StrThresh=="$")
N=names(InitialData)
WhichVar=which(N==substr(ThreshName,1,LocDlrSgn-1))
MPlusThresh=substr(ThreshName,LocDlrSgn+1,nchar(ThreshName))

TO=paste0(Directry,"/",FileName)
BigFile=read.delim(paste0(TO,"/","MPLUS",DataName,".txt"),sep="\t")
LineNums=sapply(seq(1,nrow(BigFile)),list)
NewFile=sapply(LineNums,SpaceRowRead,BigFile)
NewFile=t(NewFile)

OrigData=read.delim(paste0(TO,"/",DataName,".dat"),sep="\t")
OrigDataMTCHIndx=which(is.na(match(OrigData[,WhichVar],NADes)==TRUE))

MPlusMatchThreshIndex=suppressWarnings(which(match(as.numeric(NewFile[,WhichVar]),as.numeric(MPlusThresh))==1))
IDMPlusData=NewFile[MPlusMatchThreshIndex,ncol(NewFile)]

MTCHs=unlist(sapply(sapply(IDMPlusData,list),match,as.character(OrigData[OrigDataMTCHIndx,ncol(OrigData)])))

OriginalVarName=OrigData[OrigDataMTCHIndx,WhichVar][MTCHs]
return(OriginalVarName[1])}
