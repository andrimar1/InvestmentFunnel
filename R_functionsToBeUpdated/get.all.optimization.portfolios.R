get.all.optimization.portfolios<-function(assetReturns,nAssets,transactionCosts,criteria){

  port = list()
  path = paste("C:/Users/Emil/Dropbox (Schantz)/HDF/Afsluttende opgave/Data/weights_opt_n",nAssets,"_",criteria,".csv", sep ="")
  weightsport = read.csv(path,stringsAsFactors=TRUE,header = TRUE,check.names=FALSE)
  weightsport[is.na(weightsport)] <- 0
  weightsport[[1]] <- as.Date(weightsport[[1]] ,format="%Y-%m-%d")
  
  
  for (i in 1:8){
    
    if (i == 1){
      lambda = 0.02
    }else if (i == 3){
      lambda = 0.98
    }else{
      lambda = 0.5
    }
    
    
    risk = paste("R",i,sep="")
    weights = weightsport[weightsport[,2]==risk,][,-2]
    weights = as.xts(weights[,-1],order.by=weights[,1])
    
    w= weights
    
    #w = merge(w, index(assetReturns[,1]), all = TRUE) # converts to daily weights
    #w <- na.locf(w)

    assetsport=names(weights)
    temp=Return.portfolio(R = assetReturns[,assetsport],weights=w[,], verbose = T)
    port[[i]]=portfolio.return.transactionCosts(temp,transactionCosts)
    
  } 
  return(port)

}
