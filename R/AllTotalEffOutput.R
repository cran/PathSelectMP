AllTotalEffOutput <-
function(AllDat,GreaterThanNum=0,Directry=getwd()){
AllFileNames1=sapply(AllDat,OnlyNumberElement,1)
Tots1=lapply(AllFileNames1,CreateTotalEffMat,Directry)
AllTotEffOutput1=CreateTotalSummary(Tots1,GreaterThanNum)
return(AllTotEffOutput1)}
