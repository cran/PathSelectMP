NumEndFile <-
function(NameoFile,pattern1,pattern2){
LocUse=grep(NameoFile,pattern=pattern1)
Loc=gregexpr(pattern =pattern2,NameoFile[LocUse])[[1]][1]
num=substr(NameoFile[LocUse],1,(Loc-2))
return(as.numeric(num))
}
