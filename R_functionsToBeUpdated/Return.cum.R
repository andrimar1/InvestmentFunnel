Return.cum <- function (R, geometric = TRUE,method = "log" ) {
  if (is.vector(R)) {
    R = na.omit(R)
    if (!geometric) {
      if (method == "simple" || method == "discrete") {return(sum(R))}
      if (method == "compound" || method == "log") {return(sum(exp(R)))}
    }else{
      if (method == "simple" || method == "discrete") {return(prod(1 + R) - 1)}
      if (method == "compound" || method == "log") {return(exp(sum(R))-1)}
    }
  } else{
    R = checkData(R, method = "matrix")
    result = apply(R, 2, Return.cum, geometric = geometric, method = method)
    dim(result) = c(1, NCOL(R))
    colnames(result) = colnames(R)
    rownames(result) = "Cumulative Return"
    return(result)
  }
}
  
