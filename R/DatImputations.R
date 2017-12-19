DatImputations <-
function(InitialData,ImputeSeed,NADes,DataFileNameS,WhichCat,WhichImpute,WhichRowsImp,AllMethods){
x2=InitialData
for(i in 1:ncol(InitialData)){
is.na(x2[which(x2[,i]==NADes[1]),i])=TRUE
}
for(i in 1:length(WhichCat)){
if(WhichCat[i]==1){
x2[,i]=as.factor(x2[,i])
}
}
METHOD=rep("",ncol(x2))
for(i in 1:length(WhichCat)){
if(WhichImpute[i]==1){
if(WhichCat[i]==1){
Catnum=NumCat(i,InitialData,NADes)
if(Catnum>2){
METHOD[i]=AllMethods[2]
}
if(Catnum<=2){
METHOD[i]=AllMethods[1]
}
}else{
METHOD[i]=AllMethods[3]
}
}
}
y<-mice(x2,seed=ImputeSeed,method=METHOD)
AllImputed<-y$imp
NewImp=lapply(AllImputed,OnlyNumberElement,y$iteration)
SaveX=x2
for(k in 1:length(NewImp)){
if(length(NewImp[[i]])!=0){
IsNa=which(is.na(x2[,k])==TRUE)
MTCH=match(IsNa,WhichRowsImp)
MTCH=MTCH[which(is.na(MTCH)==FALSE)]
MTCH2=match(IsNa,WhichRowsImp[MTCH])
MTCH2=MTCH2[which(is.na(MTCH2)==FALSE)]
KeepRowsImp=WhichRowsImp[MTCH]
SaveX[KeepRowsImp,k]=NewImp[[k]][MTCH2]
}
}
ID=seq(1,nrow(SaveX))
SaveX=cbind(SaveX,ID)
for(i in 1:ncol(SaveX)){
SaveX[,i]=as.character(SaveX[,i])
SaveX[which(is.na(SaveX[,i])==TRUE),i]=as.character(NADes)
}
write.table(SaveX,DataFileNameS,sep="\t",row.names=FALSE,col.names=FALSE)
return()}
