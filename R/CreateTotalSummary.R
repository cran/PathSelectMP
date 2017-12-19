CreateTotalSummary <-
function(AllTots,GreaterThanNum){
AllEffs=lapply(AllTots,OnlyNumberElement,1)
AllSE=lapply(AllTots,OnlyNumberElement,2)
AllPVal=lapply(AllTots,OnlyNumberElement,3)
AllTCount=lapply(AllTots,OnlyNumberElement,4)
AllSum=matrix(c(rep(0,ncol(AllEffs[[1]])*nrow(AllEffs[[1]]))),nrow=nrow(AllEffs[[1]]))
AllSumSE=matrix(c(rep(0,ncol(AllEffs[[1]])*nrow(AllEffs[[1]]))),nrow=nrow(AllEffs[[1]]))
AllSumPVal=matrix(c(rep(0,ncol(AllEffs[[1]])*nrow(AllEffs[[1]]))),nrow=nrow(AllEffs[[1]]))
AllSumCount=matrix(c(rep(0,ncol(AllEffs[[1]])*nrow(AllEffs[[1]]))),nrow=nrow(AllEffs[[1]]))
AllminPVal=matrix(c(rep(0,ncol(AllEffs[[1]])*nrow(AllEffs[[1]]))),nrow=nrow(AllEffs[[1]]))
AllmaxPVal=matrix(c(rep(0,ncol(AllEffs[[1]])*nrow(AllEffs[[1]]))),nrow=nrow(AllEffs[[1]]))
AllmedPVal=matrix(c(rep(0,ncol(AllEffs[[1]])*nrow(AllEffs[[1]]))),nrow=nrow(AllEffs[[1]]))

for(i in 1:length(AllEffs)){
AllSum=AllSum+AllEffs[[i]]
AllSumSE=AllSumSE+AllSE[[i]]
AllSumPVal=AllSumPVal+AllPVal[[i]]
AllSumCount=AllSumCount+AllTCount[[i]]
}
AvgTotPVal=(AllSumPVal)/(AllSumCount+.0000001)
AvgSEVal=AllSumSE/(AllSumCount+.0000001)
AvgEff=AllSum/(AllSumCount+.0000001)
for(i in 1:nrow(AvgEff)){
for(j in 1:ncol(AvgEff)){
if(AllSumCount[i,j]<=GreaterThanNum){
AvgTotPVal[i,j]=0
AvgSEVal[i,j]=0
AvgEff[i,j]=0
}
}
}
for(i in 1:nrow(AvgEff)){
for(j in 1:nrow(AvgEff)){
if(AllSumCount[i,j]>GreaterThanNum){
PValStore=c()
for(k in 1:length(AllPVal)){
PValStore=c(PValStore,AllPVal[[k]][i,j])
}
AllminPVal[i,j]=min(PValStore)
AllmaxPVal[i,j]=max(PValStore)
AllmedPVal[i,j]=median(PValStore)
}
}
}
return("TotalEffects"=c("Count"=list(AllSumCount),"AverageEffects"=list(AvgEff),"AverageStandardError"=list(AvgSEVal),"AveragePVal"=list(AvgTotPVal),"MinPval"=list(AllminPVal),"MaxPVal"=list(AllmaxPVal),"MedianPVal"=list(AllmedPVal)))}
