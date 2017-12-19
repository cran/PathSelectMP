AllBackwardSelect <-
function(AllNames,Directry=getwd(),PSig=0.05){
lapply(sapply(AllNames,list),MPlusBackwardSelect,Directry,PSig)
return()}
