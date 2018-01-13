RF<-function(spread.annual = 0){
  RF <- read.csv("C:/Users/Emil/Dropbox (Schantz)/HDF/Afsluttende opgave/Data/1yEuroBondYield_ANNUALIZED.csv",stringsAsFactors=TRUE,header = TRUE,check.names=FALSE)
  RF[[1]] <- as.Date(RF[[1]],format="%Y-%m-%d")
  RF.annual<-as.xts(RF[,-1]+spread.annual,order.by=RF[,1])
  RF.daily = RF.annual
  
  for (year in 2007:2016){
  y = paste(year)
  RF.daily[y,] = (1+RF.annual[y,])^(1/length(RF.annual[y,]))-1
  }
  names(RF.daily)= "1YDaily"
  return(RF.daily)
}
