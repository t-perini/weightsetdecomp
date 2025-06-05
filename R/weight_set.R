#' Initialize the data frame for weights in weight set
#' 
#' Returns a data structure to organize weights equally spaced in the weight set. 
#' For 3 input scores/metrics, the weights are labeled by column as lambda1, lambda2, lambda3,
#' where lambda1 and lambda2 correspond to the right triangle representation. 
#' Every row represents one weight vector. All weights are nonnegative. 
#' All weight vectors sum to one. These weights are recommended to be used for all computations.
#' These weights may also be used for visualization (lambda1 and lambda2 only), but note that it
#' is a biased representation since the distance between vertices of the triangle are not uniform.
#' 
#' @param stepsize The step size for the grid of equally spaced points. 
#' Smaller values will increase precision but requires more time for computation. 
#' Weights are between 0 and 1 with sum of 1.  
#' @return A data frame where each row represents one grid point in the weight set.
#' @export 
#' 
#' @examples
#' Lambda <- weight_set() 
weight_set <- function(stepsize=0.01) {
  s = seq(0,1,stepsize)
  df = expand.grid(s,s)
  df = df[df$Var1+df$Var2<=1,]
  Lambda = data.frame(lambda1=df$Var1, lambda2=df$Var2)
  Lambda$lambda3 = 1-Lambda$lambda1-Lambda$lambda2
  return(Lambda)
}