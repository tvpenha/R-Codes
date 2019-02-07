# Apply function 'fun' to object 'x' over every 'by' columns
byapply <- function(x, by, fun, ...)
{
  # Create index list
  if (length(by) == 1)
  {
    nc <- ncol(x)
    split.index <- rep(1:ceiling(nc / by), each = by, length.out = nc)
  } else # 'by' is a vector of groups
  {
    nc <- length(by)
    split.index <- by
  }
  index.list <- split(seq(from = 1, to = nc), split.index)
  
  # Pass index list to fun using sapply() and return object
  sapply(index.list, function(i)
  {
    do.call(fun, list(x[, i], ...))
  })
}

# calculate mean by 10 columns

CDD_rcp45_mean = as.data.frame(byapply(CDD_rcp45, 10, rowMeans))

CDD_rcp85_mean = as.data.frame(byapply(CDD_rcp85, 10, rowMeans))
