my.boxplot=function (R, names = TRUE, as.Tufte = FALSE, sort.by = c(NULL, 
                                                         "mean", "median", "variance"), colorset = "black", symbol.color = "red", 
          mean.symbol = 1, median.symbol = "|", outlier.symbol = 1, 
          show.data = NULL, add.mean = TRUE, sort.ascending = FALSE, 
          xlab = "Return", main = "Portfolio Return Distribution", 
          element.color = "darkgray", ...) 
{
  R = checkData(R, method = "data.frame")
  columns = ncol(R)
  rows = nrow(R)
  columnnames = colnames(R)
  column.order = NULL
  sort.by = sort.by[1]
  op <- par(no.readonly = TRUE)
  if (names) {
    par(mar = c(5, 5.5, 4, 2) + 0.1)
  }
  if (length(colorset) < columns) 
    colorset = rep(colorset, length.out = columns)
  if (length(symbol.color) < columns) 
    symbol.color = rep(symbol.color, length.out = columns)
  if (length(mean.symbol) < columns) 
    mean.symbol = rep(mean.symbol, length.out = columns)
  means = sapply(R, mean, na.rm = TRUE)
  switch(sort.by, mean = {
    column.order = order(means)
    ylab = paste("Sorted by Mean", sep = "")
  }, median = {
    medians = sapply(R, median, na.rm = TRUE)
    column.order = order(medians)
    ylab = paste("Sorted by Median", sep = "")
  }, variance = {
    variances = sapply(R, var, na.rm = TRUE)
    column.order = order(variances)
    ylab = paste("Sorted by Variance", sep = "")
  }, {
    column.order = 1:columns
    ylab = paste("Unsorted", sep = "")
  })
  if (as.Tufte) {
    boxplot(R[, column.order], horizontal = TRUE, names = names, 
            main = main, xlab = xlab, ylab = "", pars = list(boxcol = "white", 
                                                             medlty = "blank", medpch = median.symbol, medlwd = 2, 
                                                             medcex = 0.8, medcol = colorset[column.order], 
                                                             whisklty = c(1, 1), whiskcol = colorset[column.order], 
                                                             staplelty = "blank", outpch = outlier.symbol, 
                                                             outcex = 0.5, outcol = colorset[column.order]), 
            axes = FALSE, ...)
  }
  else {
    boxplot(R[, column.order], horizontal = TRUE, names = names, 
            main = main, xlab = xlab, ylab = "", pars = list(boxcol = colorset[column.order], 
                                                             medlwd = 1, medcol = colorset[column.order], 
                                                             whisklty = c(1, 1), whiskcol = colorset[column.order], 
                                                             staplelty = 1, staplecol = colorset[column.order], 
                                                             staplecex = 0.5, outpch = outlier.symbol, outcex = 0.5, 
                                                             outcol = colorset[column.order]), axes = FALSE,ylim=c(-0.1,0.1) ,
            boxwex = 0.6, ...)
  }
  if (!is.null(show.data)) {
    highlight.color = 1:24
    for (item in show.data) {
      points(as.vector(R[item, column.order]), 1:columns, 
             col = highlight.color[item])
    }
  }
  if (add.mean) 
    points(means[column.order], 1:columns, pch = mean.symbol[column.order], 
           col = symbol.color[column.order])
  if (names) {
    labels = columnnames
    axis(2, cex.axis = 0.8, col = element.color, labels = labels[column.order], 
         at = 1:columns, las = 1)
  }
  else {
    labels = ""
    axis(2, cex.axis = 0.8, col = element.color, labels = labels[column.order], 
         at = 1:columns, las = 1, tick = FALSE)
  }
  axis(1, cex.axis = 0.8, col = element.color)
  box(col = element.color)
  abline(v = 0, lty = "solid", col = element.color)
  par(op)
}