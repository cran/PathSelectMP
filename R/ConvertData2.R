ConvertData2 <-
function(List1,List2,jjlist1){
NewList=c()
for(i in 1:length(List1)){
NewList=c(NewList,list(c("Name"=list(List1[[i]]),"DataName"=list(List2[[i]]),"DirectEffect"=list(jjlist1[[i]]))))
}
return(NewList)
}
