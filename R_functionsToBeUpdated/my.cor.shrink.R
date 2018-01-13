my.cor.shrink<-function (x, lambda, w, verbose = TRUE) 
{
  return(my.powcor.shrink(x = x, alpha = 1, lambda = lambda, w = w, 
                       verbose = verbose))
}