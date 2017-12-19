PathNames <-
function(rowNum,InputInitializeMat){
ColOnes=which(InputInitializeMat[rowNum,]==1)
if(length(ColOnes)!=0){
PName=names(InputInitializeMat)[ColOnes]
PName=paste(PName,collapse=" ")
PName=paste(row.names(InputInitializeMat)[rowNum],PName,sep=" ON ")
PName=paste0(PName,";","\n")
}else{
PName=""
}


return(PName)
}
