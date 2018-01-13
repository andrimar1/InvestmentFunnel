STARR.Ratio<-function (R, geometric = TRUE, p = 0.95,method="historical") 
  {
  zeroRIds=NULL
  if (any(apply(R==0,2,all))) {
    zeroRNames <- colnames(R[,which(apply(R==0,2,all))])
    zeroRIds <- which( colnames(R)%in%zeroRNames )
  }


  
    Rf = 0
    scale = NA
    R = checkData(R)
    if (!is.null(dim(Rf))) 
      Rf = checkData(Rf)
    if (is.na(scale)) {
      freq = periodicity(R)
      switch(freq$scale, minute = {
        stop("Data periodicity too high")
      }, hourly = {
        stop("Data periodicity too high")
      }, daily = {
        scale = 252
      }, weekly = {
        scale = 52
      }, monthly = {
        scale = 12
      }, quarterly = {
        scale = 4
      }, yearly = {
        scale = 1
      })
    }
    sr <- function(R, Rf, scale,p,method,geometric) {
      xR = Return.excess(R, Rf)
      xR.annualized = Return.annualized(xR, scale = scale, geometric = geometric)
      if (geometric) {
         expectedReturn = (1+xR.annualized)^(1/scale)
      } else {
        expectedReturn = xR.annualized / scale
      }
       
      ES = ES(R[,],p,method)
      
      SR = expectedReturn / ES
      SR
    }
    srZero <- function(R) {
      SR = 0
      SR
    }
    if(is.null(zeroRIds))
    {
      result = sapply(R[,], sr, Rf = Rf, scale = scale,p,method,geometric)
    }else{
      result1 = sapply(R[,-zeroRIds], sr, Rf = Rf, scale = scale,p,method,geometric)
      result2 = sapply(R[,zeroRIds], srZero)
      newdata = c(result1,result2)
      result = newdata[names(R)] 
    }
    dim(result) = c(1, NCOL(R))
    colnames(result) = colnames(R)
    rownames(result) = paste("Annualized STARR Ratio (Rf=", 
                             round(mean(Rf) * scale * 100, 1), "%)", sep = "")
    return(result)
}

