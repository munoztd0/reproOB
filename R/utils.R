# -------------------------------------- Miscellaneous  ----------------------------------------------------------

imput = function(x) {
  x[is.na(x)] <- 0 
return(x)
  }
scale2 <- function(x, na.rm = TRUE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm) # global functions