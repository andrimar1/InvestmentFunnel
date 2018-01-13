# function to create template xts object
template <- function(xlist) {
  # find set of unique column names from all objects
  cn <- unique(unlist(lapply(xlist, colnames)))
  # create template xts object
  # using a date that doesn't occur in the actual data
  minIndex <- do.call(min, lapply(xlist, function(x) index(x[1L,])))
  # template object
  xts(matrix(0,1,length(cn)), minIndex-1, dimnames=list(NULL, sort(cn)))
}

# function to apply to each xts object
proc <- function(x, template) {
  # columns we need to add
  neededCols <- !(colnames(template) %in% colnames(x))
  # merge this object with template object, filling w/zeros
  out <- merge(x, template[,neededCols], fill=0)
  # reorder columns (NB: merge.xts always uses make.names)
  # and remove first row (from template)
  out <- out[-1L,make.names(colnames(template))]
  # set column names back to desired values
  # (using attr<- because dimnames<-.xts copies)
  attr(out, "dimnames") <- list(NULL, colnames(template))
  # return object
  out
}