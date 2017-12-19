AllSummary2 <-
function(AllNames,Directry=getwd(),GreaterThanNum=0){
LL1=lapply(AllNames,CreateSummaryMats,OutputSE=FALSE,OutputPVal=FALSE,Directry=Directry,OutputFinalMat=TRUE)
LL1se=lapply(AllNames,CreateSummaryMats,OutputSE=TRUE,OutputPVal=FALSE,Directry=Directry,OutputFinalMat=TRUE)
LL1p=lapply(AllNames,CreateSummaryMats,OutputSE=FALSE,OutputPVal=TRUE,Directry=Directry,OutputFinalMat=TRUE)

LLL=CountImputedEffect(LL1,LL1se,LL1p,GreaterThanNum)
LLL1=LLL[[1]]
LLm1=LLL[[2]]
LLse1=LLL[[3]]
LLmeanP1=LLL[[4]]
LLminP1=LLL[[5]]
LLmaxP1=LLL[[6]]
LLmedP1=LLL[[7]]

return(c(list("DirectEffects"=LL1),list("Average"=LLL)))}
