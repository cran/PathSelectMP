AverageRRs <-
function(ListORatioMats,GreaterThanCountNum){

MakeThreshList<-function(ThreshL,RowYN){
if(RowYN==0){
RetV=names(ThreshL)

}else{
RetV=row.names(ThreshL)
}
return(RetV)}

AllThreshNames<-unique(unlist(lapply(ListORatioMats,MakeThreshList,1)))
AllIndNames<-unique(unlist(lapply(ListORatioMats,MakeThreshList,0)))
OldInd=AllIndNames
AccountCat=as.numeric(sapply(sapply(AllIndNames,grep,AllThreshNames),length))
AccountG1Ind=which(AccountCat>1)
AFTER=0
if(length(AccountG1Ind)!=0){
for(i in 1:length(AccountG1Ind)){
for(j in 2:AccountCat[AccountG1Ind]){
AFTER=AFTER+(AccountG1Ind[i])
AllIndNames=append(AllIndNames,values=OldInd[AccountG1Ind[i]],after=AFTER)
AFTER=AFTER+1
}
}
}
TotalColNum=length(AccountCat)+sum(AccountCat[AccountCat>0]-1)

AllRatioMat=matrix(rep(0,length(AllThreshNames)*length(AllIndNames)),nrow=length(AllThreshNames))
AllCountMat=matrix(rep(0,length(AllThreshNames)*length(AllIndNames)),nrow=length(AllThreshNames))

for(i in 1:length(ListORatioMats)){
N=names(ListORatioMats[[i]])
RN=row.names(ListORatioMats[[i]])
for(j in 1:nrow(ListORatioMats[[i]])){
Row=which(AllThreshNames==RN[j])
k=1
while(k<=ncol(ListORatioMats[[i]])){
Col=which(AllIndNames==N[k])
for(l in 1:length(Col)){
AllRatioMat[Row,(Col[l])]=AllRatioMat[Row,(Col[l])]+ListORatioMats[[i]][j,k]
if(abs(ListORatioMats[[i]][j,k])>0){
AllCountMat[Row,(Col[l])]=AllCountMat[Row,(Col[l])]+1
}
k=k+1
}
}
}
}
AllRatioMat=((AllRatioMat)/(AllCountMat+.0000000000001))
GreaterThan=AllCountMat>GreaterThanCountNum
AllRatioMat=AllRatioMat*GreaterThan
AllRatioMat=data.frame(AllRatioMat)
names(AllRatioMat)=AllIndNames
row.names(AllRatioMat)=AllThreshNames
return(c("AverageRiskRatios"=list(AllRatioMat),"CountInAverage"=list(AllCountMat)))
}
