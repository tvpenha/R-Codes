# Function to calculate mean in a layer stacking raster
# Repeat in a sequence of layers the function mean "n" times

rMean <- calc( rasterfile , fun = function(x){ by(x , c( rep(seq(1:45), times = 31)) , mean, na.rm=TRUE ) }  )