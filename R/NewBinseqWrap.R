NewBinseqWrap <-
function(n,PossibleCoefs){

NewBinseq<-function(n,v=c(),PossibleCoefs){
if(n==0){
cat(v,"\n")
return()
} else {
for(i in 1:length(PossibleCoefs[[n]])){
NewBinseq(n-1,v=c(v,PossibleCoefs[[n]][i]),PossibleCoefs)
}
}
}
L=capture.output(NewBinseq(n,v=c(),PossibleCoefs))
L=strsplit(L," ")
L=lapply(L,as.numeric)
return(L)}
