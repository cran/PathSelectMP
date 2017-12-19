AddOnINDStatements <-
function(MeanDirectList,PasteIND){
x<-MeanDirectList
Keep=c()
Keep2=c()
PasteInd=PasteIND
binseq <- function (v=c(),start,Nz,x){
if(length(Nz)<1){
cat((v),"\n")
return ()
} else {
StartPos=which(row.names(x)==start)
Nz=which(abs(x[StartPos,])>0.001)
for(i in 1:length(Nz)){
cat((v),"\n")
Rn=row.names(x)
binseq(v=c(v ,start),start=Rn[Nz[i]],Nz,x)

}
}
} 
if(PasteIND==1){
for(j in 1:length(names(x))){
StoreB=capture.output(binseq(start=names(x)[j],Nz=c(3,4),x=x))
Keep=unique(StoreB)
Keep=Keep[-c(1,2)]
Keep2=c(Keep2,Keep)
}

hh1=unique(sapply(Keep2,strsplit," "))

x1<-mapply(OnlyNumberElement,hh1,1)
x2<-mapply(OnlyNumberElement,hh1,sapply(hh1,length))
}else{
x1=c()
x2=c()
for(i in 1:nrow(MeanDirectList)){
x2Add=names(MeanDirectList)[which(abs(MeanDirectList[i,])>0)]
x2<-c(x2,x2Add)
x1<-c(x1,rep(names(MeanDirectList)[i],length(x2Add)))
}
}
x=matrix(c(x1,rep("IND",length(x1)),x2),nrow=length(x2))
x=unique(x)
x=split(x,1:nrow(x))
return(x)}
