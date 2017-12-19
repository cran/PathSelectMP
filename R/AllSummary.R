AllSummary <-
function(AllNames,Directry=getwd(),GreaterThanNum=0,PasteIND=1){
yyy=AllSummary2(AllNames)
IndStatements=lapply(yyy[[1]],AddOnINDStatements,PasteIND)
return(c(list("Average"=yyy[[2]]),list("INDStatements"=IndStatements)))}
