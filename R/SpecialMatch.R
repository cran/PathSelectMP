SpecialMatch <-
function(ListO,DelL){
for(i in 1:length(ListO)){
ListO[[i]]=ListO[[i]][is.na(DelL[[i]])==TRUE]
}
return(ListO)}
