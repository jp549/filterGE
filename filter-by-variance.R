filter.GE <- function(E, quan=0.5, var.fun= 'var'){
  # created by jp549
  # "Wed Mar  7 11:24:49 2018"
  
  # An R function to filter a matrix or data.frame by variance
  # E is a matrix or data.frame of gene expression values
  # quan is the quantile you want to filter. Default = 0.5 ie retain 50% most variable genes 
  # var.fun must be an R function that measures spread eg 'var' or 'IQR'
  
  if (!inherits(E, what=c("matrix", "data.frame"))){
    stop('E must be a matrix or a data.frame')
  }
  
  warning("Genes must be in rows, samples must be in columns")
  
  if ( quan <= 0 | quan >= 1 ){
    stop('quan must be greater than 0 and less than 1')
  }
  
  x <- apply(E, 1, FUN= var.fun, na.rm=T)
  
  var.cutoff <- quantile(vars, probs= quan)
  
  ind.keep <- which(x > var.cutoff)
  
  if(length(ind.keep) == 0){
    stop("All genes were removed: change quan")
  }
    
  E.new <- E[ind.keep, ]  
  
  E.new
  
  #eof
  
}