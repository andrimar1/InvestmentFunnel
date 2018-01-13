my.powcor.shrink = function (x, alpha, lambda, w, verbose = TRUE) 
{
  if (missing(alpha)) 
    stop("Please specify the exponent alpha!")
  x = as.matrix(x)
  powr = my.pvt.powscor(x = x, alpha = alpha, lambda = lambda, 
                     w = w, verbose = verbose)
  return(powr)
}