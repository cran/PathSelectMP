ConvertData3 <-
function(N1,N2,List1){
NewList=c()
for(i in 1:length(List1)){
NewList=c(NewList,list(c((paste0(N1,List1[i])),(paste0(N2,List1[i])),c((List1[i])))))
}
return(NewList)}
