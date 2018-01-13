#creates a portfolio balanced between assetclasses. NB not rebalanced within each class

portfolio.fixedAssetclass <- function(R,transactionCosts,rebalance_on,classes, weights = c(w.ETFA=1,w.ETFB=0,w.ETFC=0,w.ETFE=0,w.ETFX=0,w.ETFO=0)){
  #c(w.ETFA=1,w.ETFB=0,w.CEF=0,w.ETFC=0,w.ETFE=0,w.ETFX=0,w.ETFM=0,w.ETFO=0)
  weights=1/sum(weights)*weights
  
  
  classNames <- c("Alternative Exchange-Traded Fund", "Bond Exchange-Traded Fund", "Closed-Ended Fund", "Commodity Exchange-Traded Fund", "Equity Exchange-Traded Fund","Mixed Asset Exchange-Traded Fund","Money Market Exchange-Traded Fund","Other Exchange-Traded Fund")

  ETFA = names(classes[,which(classes[2,]== "Alternative Exchange-Traded Fund")])
  ETFB = names(classes[,which(classes[2,]== "Bond Exchange-Traded Fund")])
  #CEF = names(classes[,which(classes[2,]== "Closed-Ended Fund")])
  ETFC = names(classes[,which(classes[2,]== "Commodity Exchange-Traded Fund")])
  ETFE = names(classes[,which(classes[2,]== "Equity Exchange-Traded Fund")])
  ETFX = names(classes[,which(classes[2,]== "Mixed Asset Exchange-Traded Fund")])
  #ETFM = names(classes[,which(classes[2,]== "Money Market Exchange-Traded Fund")])
  ETFO = names(classes[,which(classes[2,]== "Other Exchange-Traded Fund")])
  
  
  portfolio.ETFA =Return.portfolio(R[,ETFA])
  portfolio.ETFB =Return.portfolio(R[,ETFB])
  #portfolio.CEF = Return.portfolio(R[,CEF])
  portfolio.ETFC =Return.portfolio(R[,ETFC])
  portfolio.ETFE =Return.portfolio(R[,ETFE])
  portfolio.ETFX =Return.portfolio(R[,ETFX])
  #portfolio.ETFM =Return.portfolio(R[,ETFM])
  portfolio.ETFO= Return.portfolio(R[,ETFO])
  portfolio = Return.portfolio(cbind.xts(portfolio.ETFA,portfolio.ETFB,portfolio.ETFC,
                                         #portfolio.CEF,portfolio.ETFM,
                                         portfolio.ETFE,portfolio.ETFX,portfolio.ETFO),
                                         weights=weights,rebalance_on=rebalance_on,verbose=T)

  portfolio= portfolio.return.transactionCosts(portfolio,transactionCosts)
  return(portfolio)
} 
