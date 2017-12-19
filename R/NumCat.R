NumCat <-
function(ColDes,DataMat,NADes){
#NADes is the value for NA often -99
ColDes=ColDes[[1]]
DataVec=DataMat[,ColDes]
CatNum=length(unique(DataVec[which((DataVec)!=NADes)]))
return(CatNum)}
