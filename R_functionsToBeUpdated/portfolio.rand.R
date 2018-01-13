##generates several (random) 1/n portfolios from N assetreturns
portfolio.rand <- function(assetReturns,nAssets, nPortfolios, rebalance_on, transactionCosts=0){
  set.seed(1234)
  for (i in 1:(nPortfolios/100)){
    for (port in 1:100){
      assetIds = sample(1:length(assetReturns[1,]),nAssets)
      portfolio=Return.portfolio(assetReturns[,assetIds], rebalance_on=rebalance_on,verbose=TRUE)
      portfolio=portfolio.return.transactionCosts(portfolio,transactionCosts)
      if(port==1){
        portfolios=portfolio  
      }else{
        portfolios$returns=merge.xts(portfolios$returns,portfolio$returns) 
        portfolios$contribution =merge.xts(portfolios$contribution,portfolio$contribution)
        portfolios$BOP.Weight =merge.xts(portfolios$BOP.Weight,portfolio$BOP.Weight)
        portfolios$EOP.Weight =merge.xts(portfolios$EOP.Weight,portfolio$EOP.Weight)
        portfolios$BOP.Value =merge.xts(portfolios$BOP.Value,portfolio$BOP.Value)
        portfolios$EOP.Value =merge.xts(portfolios$EOP.Value,portfolio$EOP.Value)
      }
      #print(port)
    }
    if(i==1){
      portfolios2=portfolios  
    }else{
      portfolios2$returns=merge.xts(portfolios2$returns,portfolios$returns) 
      portfolios2$contribution =merge.xts(portfolios2$contribution,portfolios$contribution)
      portfolios2$BOP.Weight =merge.xts(portfolios2$BOP.Weight,portfolios$BOP.Weight)
      portfolios2$EOP.Weight =merge.xts(portfolios2$EOP.Weight,portfolios$EOP.Weight)
      portfolios2$BOP.Value =merge.xts(portfolios2$BOP.Value,portfolios$BOP.Value)
      portfolios2$EOP.Value =merge.xts(portfolios2$EOP.Value,portfolios$EOP.Value)
    }
    print(i*port)
    
  }
  return(portfolios2)
}