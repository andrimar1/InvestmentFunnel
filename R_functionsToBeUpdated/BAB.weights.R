# Clustering

BAB.weights <- function(prices, clust.no, criteria = "sharpe"){
  
  logr<-na.omit(Return.calculate(prices, method = "log"))
  logr3d <- na.omit(diff(log(prices), lag=3))
  
  members = clustering(prices,cor.method="spearman",clust.method="complete",clust.no = clust.no, criteria = "sharpe",returnMembers=TRUE)
  
  cluster.names= list()
  logr_m= list()
  logr3d_m = list()
  
  n = 0
  for(i in 1:nAssets){
    cluster.names[[i]] <-names(which(members == i))   # the names of the assets in clusters i  
    idsInCluster = length(cluster.names[[i]])
    if (idsInCluster>1){
      n=n+1 #only clusters with more than one element will be used
      logr_m[[i]] = Return.portfolio(logr[,cluster.names[[i]]])
      logr3d_m[[i]] = Return.portfolio(logr3d[,cluster.names[[i]]])
      
      b=c(rep(NA, idsInCluster))
      names(b)= cluster.names[[i]]
      w=0.6 # shrink factor
        for(j in 1:idsInCluster){
          ticker = cluster.names[[i]][j]
          b[j] = w*cor(logr3d_m[[i]],logr3d[,ticker]) *sd(logr[,ticker])/ sd(logr_m[[i]])+(1-w)
        }
      
      b=sort(b[b>0])
      median = median(b)
      #low.beta = sort(b[b<=median],decreasing = FALSE)
      low.beta = sort(b[b<median],decreasing = FALSE)
      z_l = length(low.beta):1
      names(z_l)= names(low.beta)
      w_l=z_l/sum(z_l)
      
      high.beta = sort(b[b>median],decreasing = FALSE)
      z_h = 1:length(high.beta)
      names(z_h)= names(high.beta)
      
      w_h=z_h/sum(z_h)
      
      beta_high.beta.port=w_h*high.beta
      factor.high.beta.port=1/sum(beta_high.beta.port)
      weight.high.beta.port = w_h*factor.high.beta.port
      sum.high.beta = sum(weight.high.beta.port)
      
      beta_low.beta.port=w_l*low.beta
      factor.low.beta.port=1/sum(beta_low.beta.port)
      weight.low.beta.port = w_l*factor.low.beta.port
      sum.low.beta = sum(weight.low.beta.port)
      
      sum(weight.high.beta.port*high.beta) #check that high portfolio has a beta of one
      sum(weight.low.beta.port *low.beta) #check that low portfolio has a beta of one
      if (i == 1){
        weight.high.beta.port.total =  weight.high.beta.port
        weight.low.beta.port.total =  weight.low.beta.port 
      }else{
        weight.high.beta.port.total = c(weight.high.beta.port.total,weight.high.beta.port)
        weight.low.beta.port.total = c(weight.low.beta.port.total,weight.low.beta.port)
      }
    }
  }
  return(list("High.Beta.Weights" = weight.high.beta.port.total/n,"Low.Beta.Weights" = weight.low.beta.port.total/n))
  
}

