ParseTotalEffects2 <-
function(OutFile,StandardError=FALSE,PVal=FALSE,Indirect=FALSE){
#For Total Effects
z1=which(match(OutFile,"Total")==1)[seq(1,length(which(match(OutFile,"Total")==1)),2)]
z=(mapply(seq,z1-5,z1+10))
AllNameTotTotIndList=OutFile[z[1:nrow(z),]]
if(Indirect==TRUE){
P1=seq(13,length(AllNameTotTotIndList),16)
if(StandardError==TRUE){
P1=seq(14,length(AllNameTotTotIndList),16)
}
if(PVal==TRUE){
P1=seq(16,length(AllNameTotTotIndList),16)
}

}else{
P1=seq(7,length(AllNameTotTotIndList),16)
if(StandardError==TRUE){
P1=seq(8,length(AllNameTotTotIndList),16)
}
if(PVal==TRUE){
P1=seq(10,length(AllNameTotTotIndList),16)
}
}
PT=as.numeric(AllNameTotTotIndList[P1])

#Indirect Effects
I1=seq(3,length(AllNameTotTotIndList),16)
I2=rep(0,length(I1)*3)
I2[which(mod(seq(1,length(I2)),3)==0)-0]=I1+2
I2[which(mod(seq(1,length(I2)),3)==0)-1]=I1+1
I2[which(mod(seq(1,length(I2)),3)==0)-2]=I1
INDNameMat=matrix(AllNameTotTotIndList[I2],byrow=TRUE,ncol=3)
PI1=seq(16,length(AllNameTotTotIndList),16)
PTI=as.numeric(AllNameTotTotIndList[PI1])

EntireF=c("TotalVals"=list(PT),"TotalEffectNames"=list(INDNameMat))
return(EntireF)}
