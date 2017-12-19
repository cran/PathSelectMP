CreateSummaryMats2 <-
function(AllDat,Directry){

FileName=AllDat[[1]]
jj=AllDat[[2]]

TO=paste(Directry,"/",FileName,sep="")
print(TO)
files <- list.files(path=TO,pattern = ".INP$")

hh=lapply(strsplit(files,"_"),NumEndFile,".INP",'INP')

hh=as.numeric(paste(hh))
LastFileName=files[which(hh==max(hh))]

NewFNum=max(hh)+1

y=paste(readLines(paste(TO,"/",LastFileName,sep="")),collapse="\n")
ys=strsplit(y,"\n")[[1]]

StartModelInd=which(ys=="MODEL INDIRECT:")
Ys2=ys
Ys2=Ys2[1:StartModelInd]

jj=unlist(jj)
jj[mod(c(1:length(jj)),3)==2]=paste0(" ",substr(jj[mod(c(1:length(jj)),3)==2],1,nchar(jj[mod(c(1:length(jj)),3)==2]))," ")
newjj=rep(0,length(jj)/3+length(jj))
newjj[mod(c(1:length(newjj)),4)==0]="\n"
newjj[mod(c(1:length(newjj)),4)!=0]=jj
newjj[mod(c(1:length(newjj)),4)==3]=paste0(substr(newjj[mod(c(1:length(newjj)),4)==3],1,nchar(newjj[mod(c(1:length(newjj)),4)==3])),";")
newjj=c("\n",newjj)
newjj=paste(newjj,collapse="")

UU=gregexpr(pattern ='_',LastFileName)[[1]]
UscoreLast=UU[length(UU)]
INPLoc=gregexpr(pattern ='.INP',LastFileName)[[1]][1]
NewLast=LastFileName
NewLast=paste(substr(NewLast,1,UscoreLast),NewFNum,substr(NewLast,INPLoc,nchar(NewLast)),sep="")

WNEWScript=paste(Ys2,collapse="\n")
WNEWScript=paste(WNEWScript,newjj,sep="")
check=strsplit(WNEWScript,"\n")[[1]]
writeLines(WNEWScript,paste(TO,"/",NewLast,sep=""))

runModels(TO,logFile="MH_RunLog.txt",showOutput=FALSE,replaceOutfile="never")
return(NewLast)
}
