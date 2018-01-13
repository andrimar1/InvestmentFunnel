clustering.itterations <- function(prices,clust.no, as.portfolio=FALSE, costs=0, lag=1, cluster_on ="yearly",criteria="sharpe")
{
  firstYear= format(index(first(prices[,1])), "%Y")
  lastYear= format(index(last(prices[,1])), "%Y")
  count = 1
  
  
  if(cluster_on =="monthly"){ #not working ATM
    ids=apply.monthly(na.omit(prices[,1:clust.no]),mean)
  }else{
    ids=apply.yearly(na.omit(prices[,1:clust.no]),mean)
  }
  colnames(ids)=1:clust.no
  
  for (year in firstYear:lastYear){
    if(cluster_on =="monthly"){
      for (month in c(1:12)){
        a=paste(year,month, sep = "-")
        print(a)
        ids[count,] = clustering(prices=prices[a,],clust.no = clust.no,criteria=criteria)
        count=count+1
      }   
    }else{
      a=paste(year)
      ids[count,] = clustering(prices=prices[a,],clust.no = clust.no,criteria=criteria)
      count=count+1
    }
    print(year)
  }
  
  if(as.portfolio==TRUE){
    assetReturns = Return.calculate(prices,method = "discrete")

    # cluster_on =="monthly" -------------------------------------------------------
    if(cluster_on=="monthly"){
    for (year in as.numeric(firstYear):(as.numeric(lastYear))){
      
      for (month in c(1:12)){
        if (month==12){
          a=paste(year,month, sep = "-")
          b=paste(year+1,1, sep = "-")
        }else{
          a=paste(year,month, sep = "-")
          b=paste(year,month+1, sep = "-")
         }
        if( year==lastYear && month == 12 ){
          break
        }else if(year==firstYear && month == 1 ){
        c=Return.portfolio(assetReturns[b,ids[a,]],verbose = TRUE)
        EOP.Weight = c$EOP.Weight
        BOP.Weight = c$BOP.Weight
        EOP.Value = c$EOP.Value
        BOP.Value = c$BOP.Value
        contribution = c$contribution
        returns = c$returns
        
      }else{
        d=Return.portfolio(assetReturns[b,ids[a,]],verbose = TRUE)
        
        x.EOP.Weight <- list(EOP.Weight, d$EOP.Weight)
        EOP.Weight <- do.call(rbind, lapply(x.EOP.Weight, proc, template=template(x.EOP.Weight)))
        x.BOP.Weight <- list(BOP.Weight, d$BOP.Weight)
        BOP.Weight <- do.call(rbind, lapply(x.BOP.Weight, proc, template=template(x.BOP.Weight)))
        
        x.EOP.Value <- list(EOP.Value, d$EOP.Value)
        EOP.Value <- do.call(rbind, lapply(x.EOP.Value, proc, template=template(x.EOP.Value)))
        x.BOP.Value <- list(BOP.Value, d$BOP.Value)
        BOP.Value <- do.call(rbind, lapply(x.BOP.Value, proc, template=template(x.BOP.Value)))
        
        x.contribution <- list(contribution, d$contribution)
        contribution <- do.call(rbind, lapply(x.contribution, proc, template=template(x.contribution)))
        
        returns=rbind(returns,d$returns)
        }
      }
    }
    }else{
    
    # cluster_on =="yearly" -------------------------------------------------------
    
    
    for (year in as.numeric(firstYear):(as.numeric(lastYear)-lag)){
      a=paste(year)
      b=paste(year+lag)
      if(year==firstYear){
        c=Return.portfolio(assetReturns[b,ids[a,]],verbose = TRUE)
        EOP.Weight = c$EOP.Weight
        BOP.Weight = c$BOP.Weight
        EOP.Value = c$EOP.Value
        BOP.Value = c$BOP.Value
        contribution = c$contribution
        returns = c$returns
        
      }else{
        d=Return.portfolio(assetReturns[b,ids[a,]],verbose = TRUE)
        
        x.EOP.Weight <- list(EOP.Weight, d$EOP.Weight)
        EOP.Weight <- do.call(rbind, lapply(x.EOP.Weight, proc, template=template(x.EOP.Weight)))
        x.BOP.Weight <- list(BOP.Weight, d$BOP.Weight)
        BOP.Weight <- do.call(rbind, lapply(x.BOP.Weight, proc, template=template(x.BOP.Weight)))
        
        x.EOP.Value <- list(EOP.Value, d$EOP.Value)
        EOP.Value <- do.call(rbind, lapply(x.EOP.Value, proc, template=template(x.EOP.Value)))
        x.BOP.Value <- list(BOP.Value, d$BOP.Value)
        BOP.Value <- do.call(rbind, lapply(x.BOP.Value, proc, template=template(x.BOP.Value)))
        
        x.contribution <- list(contribution, d$contribution)
        contribution <- do.call(rbind, lapply(x.contribution, proc, template=template(x.contribution)))
        
        returns=rbind(returns,d$returns)
      }
    }
    }
    # Data adjustment-------------------------------------------------------
    
    portfolio = d #initialize
    portfolio$returns = returns
    portfolio$EOP.Weight = EOP.Weight
    portfolio$BOP.Weight = BOP.Weight
    portfolio$EOP.Value = EOP.Value
    portfolio$BOP.Value = BOP.Value
    portfolio$contribution = contribution
    portfolio=portfolio.return.transactionCosts(portfolio,costs) # adjust for transactioncosts
    
  return(portfolio)  
    }
  return(ids)
}

