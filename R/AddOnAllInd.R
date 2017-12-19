AddOnAllInd <-
function(FileName,IndStatements,Directry=getwd()){

ConvertData<-function(List1,jjlist1){
NewList=c()
for(i in 1:length(List1)){
NewList=c(NewList,list(c(list(List1[[i]]),list(jjlist1[[i]]))))
}
return(NewList)}

AllDat1=ConvertData(FileName,IndStatements)
qq1=lapply(AllDat1,CreateSummaryMats2,Directry)
return()}
