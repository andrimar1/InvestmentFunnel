#' Does a clustering on data based on chosen parameters, methods and criteria
#'
#' @param prices Price data.
#' @param cor.method Method to calculate correlation .
#' @param clust.no Integer. Numbers of clusters.
#' @param criteria
#' @param returnMembers Bool. Return group memberships
#' @export
#' @examples
#' fv.single(rate=0.01,nper=20,pv=-1000)

clustering <- function(prices,cor.method="spearman",clust.method="ward.D2", clust.no, criteria = "sharpe",returnMembers=FALSE){
  logr <- na.omit(Return.calculate(prices, method = "log"))
  a=apply(logr,2,var,na.rm=T)
  logr<-logr[,a!=0]
  logr3d <- na.omit(diff(log(prices), lag=3))
  b=apply(logr3d,2,var,na.rm=T)
  logr3d<-logr3d[,b!=0]

  if (criteria=="sharpe"){
    cumr=0
    sharpe=SharpeRatio.annualized(logr)
    STARR =0
  }else if(criteria=="STARR"){
    cumr=0
    sharpe=0
    STARR =STARR.Ratio(logr)
  }else{
    cumr = Return.cumulative(logr)
    sharpe=0
    STARR=0
  }
  c<- my.cor.shrink(logr3d[,]) # always uses spearman correlation


  c[is.na(c)] <- 1
  d <- as.dist(1-c)                       # Convert the correlation coefficient to a distance
  hc <- hclust(d,method=clust.method)     # Construct the tree
  plot(hc,labels=F,)



  # colored dendrogram
  #if(FALSE){
 # source("functions/dendogram.R")
 # pdf(file="C:/Users/Emil/Dropbox (Schantz)/HDF/Afsluttende opgave/Report/figures/Dendogram_n=7.pdf",width=8,height=5)
 # par(mar=c(3,3,2,0.2),mgp=c(2,1,0), cex.main=1.6)
 # A2Rplot(hc, k = 7, boxes = FALSE, col.up = CBSLG, col.down = rep(c(CBSG,CBSB),4), main ="Dendrogram (n=7) ", lwd.up = 1, lwd.down = 1, show.labels = F)
 # mtext(side = 1, "Instruments" , line= 0)
 #   dev.off()

 #   pdf(file="C:/Users/Emil/Dropbox (Schantz)/HDF/Afsluttende opgave/Report/figures/Dendogram_n=20.pdf",width=8,height=5)
 #   par(mar=c(3,3,2,0.2),mgp=c(2,1,0), cex.main=1.6)
    #A2Rplot(hc, k = 20, boxes = FALSE, col.up = CBSLG, col.down = rep(c(CBSG,CBSB),10), main ="Dendrogram (n=20) ", lwd.up = 1, lwd.down = 1, show.labels = F)
 #   mtext(side = 1, "Instruments" , line= 0)
  #  dev.off()
  #}

  memb <- cutree(hc, k = clust.no)             # Cut the hirarchical tree to the desired number of clusters
  if ( returnMembers==TRUE)
  {return(memb)}
  Gselect <- rep(0,clust.no)  # storage
  time <- index(prices) # time parameters


  # The clusters are searched through and the best asset is chosen according to the selection criteria
  # The index "i" is a given cluster. We can evaluate clusters in a number of ways.
  # Here, I have demonstrated how we can use either maximum return or
  # minimum standard deviation as a selection criteria

  for(i in 1:clust.no){
    Gnames <- names(which(memb == i))   # the names of the assets in clusters i


    if (criteria=="BAB"){
      print("Criteria is BAB")
      logr_m = Return.portfolio(logr[,Gnames])
      logr3d_m = Return.portfolio(logr3d[,Gnames])
      beta=c(rep(NA, length(Gnames)))
      names(beta)= Gnames

      for (j in 1:length(Gnames)){
        ticker = Gnames[j]
        beta[j] = cor(logr3d_m,logr3d[,ticker]) * sd(logr[,ticker])/ sd(logr_m)
      }

      Gselect[i] <- Gnames[which.min(beta)] # Min beta is selection criteria
    }else if (criteria=="sharpe"){
      print("Criteria is sharpe")
      Gselect[i] <- Gnames[which.max(sharpe[1,Gnames[1:length(Gnames)]])] # Max sharpe ratio is selection criteria
    }else if(criteria =="STARR"){
      print("Criteria is STARR")
      Gselect[i] <- Gnames[which.min(STARR[1,Gnames[1:length(Gnames)]])] # Min STARR ratio is selection criteria
    }else{
      print("Criteria is by default  max return")
      Gselect[i] <- Gnames[which.max(cumr[1,Gnames[1:length(Gnames)]])] # Max cumultative return is selection criteria
    }

    #Gselect[i] <- Gnames[which.max(cumr[1,Gnames[1:length(Gnames)]])] # Max cumulated return is selection criteria
  }

  #if (FALSE){
  #c<- my.cor.shrink(logr3d[,Gselect]) # always uses spearman correlation
  #c[is.na(c)] <- 1
  #d <- as.dist(1-c)
  #remember to use the same linkage method as you used for constructing the tree
  #hc <- hclust(d,method=clust.method)

  #pdf(file="C:/Users/Emil/Dropbox (Schantz)/HDF/Afsluttende opgave/Report/figures/Dendogram_Beta_n=7.pdf",width=8,height=5)
  #par(mar=c(3,3,2,0.2),mgp=c(2,1,0), cex.main=1.6)
  #A2Rplot(hc, k = 7, boxes = FALSE, col.up = CBSLG, col.down = rep(c(CBSG,CBSB),4), main ="Dendrogram (Beta critrion, n=7) ", lwd.up = 1, lwd.down = 1)
  #dev.off()
  #}

  return(Gselect)

}
