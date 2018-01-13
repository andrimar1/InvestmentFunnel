##adjusts portfolio returns for transactioncosts

portfolio.return.transactionCosts <- function(results, costs){
  bop <- results$BOP.Weight #beginning of period weights
  eop <- results$EOP.Weight #end of period weights
  costs.val=costs*rowSums(abs(bop-lag(eop)), na.rm = T)
  costs.val[1]=costs
  results$returns = (1+results$returns)* (1-costs.val)-1
  print(paste("Adjusted for transactioncosts with rate =",costs))
  return(results)
}