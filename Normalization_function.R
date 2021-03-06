# Normalization function from 0 to 1 
rnorm<-function(x, minimum=0, maximum=1) {
  for(i in 1:ncol(x)){
    if(is.numeric(x[,i])){
      v_min<-min(x[,i])
      v_max<-max(x[,i])
      for(j in 1:nrow(x)){
        x[j,i]<-((x[j,i]-v_min)/(v_max-v_min))*((maximum-minimum)+minimum)
      }
    }
  }
  return(x) # Return a normalized sheet
}