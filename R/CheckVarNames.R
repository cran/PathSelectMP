CheckVarNames <-
function(InitialData){
N=names(InitialData)
G1=gregexpr("[A-Z]",N)
G2=gregexpr("[a-z]",N)
G3=gregexpr("[0-9]",N)
GTot=rep(0,length(G1))
for(i in 1:length(GTot)){
GTot=c(as.numeric(G1[[i]]),as.numeric(G2[[i]]),as.numeric(G3[[i]]))
if(as.numeric(G1[[i]])[1]!=1){
print("error variable names must start with a capital letter")
return("CheckFailed")
}
}
if(length(which(GTot<1))!=0){
GTot=GTot[-which(GTot<1)]
}
if(length(GTot)<nchar(N[i])){
print("ERROR variable names must start with a capital letter and contain only letters and numbers: no special characters")
return("CheckFailed")
}
return("Success")}
