oneOverNClustering.weights<-function(prices,clust.no,criteria){
  members = clustering(prices,cor.method="spearman",clust.method="complete",clust.no, criteria = "sharpe",returnMembers=TRUE)
  
  cluster.names= list()
  for(i in 1:nAssets){
    cluster.names[[i]] <-names(which(members == i)) 
    w = 1/length(cluster.names[[i]])*1/nAssets
    w= rep(w,length(cluster.names[[i]]))
    names(w)= cluster.names[[i]]
    
    if (i == 1){
      weights =  w
    }else{
      weights = c(weights,w)
    }
  }
  return(weights)
}