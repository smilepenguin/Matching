Nnorm <- function(n, c, L = 0){
  if (L == 0){
    return(as.numeric(format(round(rnorm(n, c[1], c[2]),c[3]), nsmall = c[3])))
  } else {
    return(as.numeric(format(round(rnorm(n, c[4], c[2]),c[3]), nsmall = c[3])))
  }
}