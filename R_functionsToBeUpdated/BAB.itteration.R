BAB.itteration<- function(prices, nAssets,firstYear,lastYear){
  BAB = list()
  factor.low.beta = list()
  factor.high.beta = list()
  weights.low.beta = list()
  weights.high.beta = list()
  assets.low.beta = c()
  assets.high.beta = c()
  i=1
  for (year in firstYear:lastYear){
    y= paste(year)
    BAB[[i]] = BAB.weights(prices = prices[y],clust.no = nAssets,criteria = "sharpe")  
    
    factor.low.beta[[i]] = sum(BAB[[i]]$Low.Beta.Weights)
    factor.high.beta[[i]] = sum(BAB[[i]]$High.Beta.Weights)
    weights.low.beta[[i]] = BAB[[i]]$Low.Beta.Weights/factor.low.beta[[i]]
    weights.high.beta[[i]] = BAB[[i]]$High.Beta.Weights/factor.high.beta[[i]]
    assets.low.beta=c(assets.low.beta,names(weights.low.beta[[i]]))
    assets.high.beta=c(assets.high.beta,names(weights.high.beta[[i]]))
    i=i+1
  }
  
  assets.low.beta = unique(assets.low.beta)
  assets.high.beta = unique(assets.high.beta)
  
  x=do.call(rbind, lapply(split(prices[,1], "months"), last))[-c(1:11)]
  x[1:length(x)]=0
  lbw=matrix(0,nrow=length(x), ncol= -1+length(assets.low.beta))
  lbw=cbind(x,lbw)
  names(lbw)=assets.low.beta
  lbf = prices[-c(1:260),1]
  lbf[1:length(lbf)]=0
  names(lbf)="LBLeverage"
  j=0
  for (year in (firstYear):(lastYear)){
    j=j+1
    l=(j-1)*12+1
    y = paste(year+1)
    assets = names(weights.low.beta[[j]])
    lbw[l,assets] = weights.low.beta[[j]]
    lbf[y]=factor.low.beta[[j]]
    if(year != lastYear)
    for(q in 1:11){
      lbw[l+q,assets] = lbw[l,assets]
    }
  }
  
  x=do.call(rbind, lapply(split(prices[,1], "months"), last))[-c(1:11)]
  x[1:length(x)]=0
  hbw=matrix(0,nrow=length(x), ncol= -1+length(assets.high.beta))
  hbw=cbind(x,hbw)
  names(hbw)=assets.high.beta
  hbf = prices[-c(1:260),1]
  hbf[1:length(hbf)]=0
  names(hbf)="HBLeverage"  
  j=0
  for (year in (firstYear):(lastYear)){
    j=j+1
    l=(j-1)*12+1
    y = paste(year+1)
    assets = names(weights.high.beta[[j]])
    hbw[l,assets] = weights.high.beta[[j]]
    hbf[y]=factor.high.beta[[j]]
    if(year != lastYear)
      for(q in 1:11){
        hbw[l+q,assets] = hbw[l,assets]
     }
  }
  lbf[1]=lbf[2]
  hbf[1]=hbf[2]
  
  return(list("Low.Beta.Weights"= lbw,"High.Beta.Weights"= hbw, "Low.Beta.Factor "=lbf,"High.Beta.Factor "=hbf ))
}