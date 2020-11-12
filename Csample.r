
Csample <- function(prob, size=n){
  return(as.numeric(sample(length(prob), size = size, replace = TRUE, prob = prob)))
}