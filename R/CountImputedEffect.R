CountImputedEffect <-
function(LL,LLse,LLPVal,GreaterThanNum){
#GreaterThanNum is the number of times over which a variable relationship should be seen in order to be used in overall mean results, 
#default is more than 0 times

Great0<-function(xx){
zz=matrix(as.numeric(abs(xx)>0.001),nrow=nrow(xx))
return(zz)}

LLL=lapply(LL,Great0)
BigLLL=matrix(rep(0,length(LLL[[1]])),nrow=nrow(LLL[[1]]))
BigLLm=matrix(rep(0,length(LLL[[1]])),nrow=nrow(LLL[[1]]))
BigLLmse=matrix(rep(0,length(LLL[[1]])),nrow=nrow(LLL[[1]]))
BigLLmeanPval=matrix(rep(0,length(LLL[[1]])),nrow=nrow(LLL[[1]]))
PValMinMat=matrix(rep(0,length(LLL[[1]])),nrow=nrow(LLL[[1]]))
PValMaxMat=matrix(rep(0,length(LLL[[1]])),nrow=nrow(LLL[[1]]))
PValMedianMat=matrix(rep(0,length(LLL[[1]])),nrow=nrow(LLL[[1]]))


for(i in 1:length(LLL)){
BigLLL=BigLLL+LLL[[i]]
BigLLm=BigLLm+LL[[i]]
BigLLmse=BigLLmse+LLse[[i]]
BigLLmeanPval=BigLLmeanPval+LLPVal[[i]]
}

for(i in 1:nrow(BigLLL)){
for(j in 1:ncol(BigLLL)){
if(BigLLL[i,j]>GreaterThanNum){
BigLLm[i,j]=BigLLm[i,j]/BigLLL[i,j]
BigLLmse[i,j]=BigLLmse[i,j]/BigLLL[i,j]
BigLLmeanPval[i,j]=BigLLmeanPval[i,j]/BigLLL[i,j]
}else{
BigLLm[i,j]=0
BigLLmse[i,j]=0
BigLLmeanPval[i,j]=0
}
}
}

for(j in 1:nrow(BigLLL)){
for(k in 1:ncol(BigLLL)){
PValVec=c()
Include=c()
for(i in 1:length(LLL)){
PValVec=c(PValVec,LLPVal[[i]][j,k])
Include=c(Include,LLL[[i]][j,k])
}
if(length(which(Include==1))>GreaterThanNum){
PValMinMat[j,k]=min(PValVec[which(Include==1)])
PValMaxMat[j,k]=max(PValVec[which(Include==1)])
PValMedianMat[j,k]=median(PValVec[which(Include==1)])
}
}
}

BigLLL=data.frame(BigLLL)
BigLLm=data.frame(BigLLm)
BigLLmse=data.frame(BigLLmse)
BigLLmeanPval=data.frame(BigLLmeanPval)
PValMinMat=data.frame(PValMinMat)
PValMaxMat=data.frame(PValMaxMat)
PValMedianMat=data.frame(PValMedianMat)

row.names(BigLLL)=row.names(LL[[1]])
row.names(BigLLm)=row.names(LL[[1]])
row.names(BigLLmse)=row.names(LL[[1]])
row.names(PValMinMat)=row.names(LL[[1]])
row.names(PValMaxMat)=row.names(LL[[1]])
row.names(PValMedianMat)=row.names(LL[[1]])
row.names(BigLLmeanPval)=row.names(LL[[1]])

names(BigLLL)=names(LL[[1]])
names(BigLLm)=names(LL[[1]])
names(BigLLmse)=names(LL[[1]])
names(PValMinMat)=names(LL[[1]])
names(PValMaxMat)=names(LL[[1]])
names(PValMedianMat)=names(LL[[1]])
names(BigLLmeanPval)=names(LL[[1]])

return(c(list("DirectEffectCounts"=BigLLL),list("MeanDirectEffects"=BigLLm),list("MeanStandardError"=BigLLmse),list("MeanPValue"=BigLLmeanPval),list("MinPVals"=PValMinMat),list("MaxPVals"=PValMaxMat),list("MedianPVals"=PValMedianMat)))}
