ParseTotalEffects <-
function(OutFile,FileName,Directry){
#For Total Effects
z1=which(match(OutFile,"Total")==1)[seq(1,length(which(match(OutFile,"Total")==1)),2)]
z=(mapply(seq,z1-5,z1+10))
AllNameTotTotIndList=OutFile[z[1:nrow(z),]]
P1=seq(10,length(AllNameTotTotIndList),16)
PT=as.numeric(AllNameTotTotIndList[P1])

#use different lists for direct and total/indirect so you can delete indirect list but keep direct effects
#For Direct Effects
Summs<-CreateSummaryMats(FileName=FileName,OutputSE=FALSE,OutputPVal=TRUE,Directry=Directry,OutputFinalMat=FALSE)
o=c()
for(i in 1:length(Summs[[1]])){
o=c(o,rep(Summs[[1]][i],length(Summs[[2]][[i]])))
}
M=matrix(c(o,unlist(Summs[[2]])),nrow=length(o))
Mn=cbind(M[,2],rep("to",length(M[,2])),M[,1])
Mn<-Mn
P2=Mn
PD=unlist(Summs[[3]])

#Indirect Effects
I1=seq(3,length(AllNameTotTotIndList),16)
I2=rep(0,length(I1)*3)
I2[which(mod(seq(1,length(I2)),3)==0)-0]=I1+2
I2[which(mod(seq(1,length(I2)),3)==0)-1]=I1+1
I2[which(mod(seq(1,length(I2)),3)==0)-2]=I1
INDNameMat=matrix(AllNameTotTotIndList[I2],byrow=TRUE,ncol=3)
PI1=seq(16,length(AllNameTotTotIndList),16)
PTI=as.numeric(AllNameTotTotIndList[PI1])

EntireF=c("TotalPVals"=list(PT),"INDPVals"=list(PTI),"INDNames"=list(INDNameMat),"DirectPVals"=list(PD),"DirectNames"=list(P2))

return(EntireF)}
