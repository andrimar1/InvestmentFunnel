output.forOptimization<- function(R){
  logr=na.omit(R)
  Gselect=gsub("\\:", "-", names(logr))
first=index(logr[1,])
last = index(logr[length(logr[,1]),])
# Output returns as .inc
  titleA = paste(first,last,'AssetReturns.inc', sep="_")
sink(titleA)
for(j in 1:length(Gselect)){
  for(i in 1:nrow(logr)){
    cat(as.character(index(logr)[i]))
    cat(".")
    cat(Gselect[j])
    cat("\t")
    cat(logr[i, j]) 
    cat("\n") 
  }  
}
sink()

titleB = paste(first,last,"ETFs.txt", sep="_")

# Save names of selected ETFs
sink(titleB)
cat('set assets /\n')
cat(Gselect, sep = ', ')
cat('\n/;')  
sink()

titlec = paste(first,last,"Dates.txt", sep="_")
# Save dates
sink(titlec)
cat('set dates /\n')
cat(noquote(as.character(index(logr))),sep=',')
cat('\n/;')  
sink()
}

