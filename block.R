# Processing Optimization 
# function to break a raster in blocks and apply a function to each block


forEachBlock <- function(raster, func){
  bs <- raster::blockSize(raster)
  
  for(i in 1:bs$n){
    cat(paste0("Processing block ", i, "/", bs$n, "\n"))
    row <- bs$row[i]
    nrows <- bs$nrows[i]
    
    block <- raster::getValues(raster, row = row, nrows = nrows)
    
    func(block, row, nrows)
  }
}

sits2015 <- raster::raster("rasterfile.tif")

forEachBlock(sits2015, function(block){
  # apply the function
  block[is.na(block)] <- 0
  # ...
})