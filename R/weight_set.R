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
#' @param num_metrics The dimension of the problem indicating total number of metrics to be aggregated 
#' (3, 4, or 5)
#' @param stepsize The step size for the grid of equally spaced points. 
#' Smaller values will increase precision but requires more time for computation. 
#' Weights are between 0 and 1 with sum of 1. 
#' @param equilateral Binary to indicate whether or not to include the equilateral transformation
#' as columns equilambda1 and equilambda2
#' @return A data frame where each row represents one grid point in the weight set.
#' @export 
#' 
#' @examples
#' Lambda <- weight_set()
#' Lambda <- weight_set(stepsize=0.05)
#' Lambda <- weight_set(num_metrics=4) 
weight_set <- function(num_metrics=3, stepsize=0.01, equilateral=TRUE) {
  s = seq(0,1,stepsize)
  
  if(num_metrics==3) {
    df = expand.grid(s,s)
    df = df[df$Var1+df$Var2<=1,]
    Lambda = data.frame(lambda1=df$Var1, lambda2=df$Var2)
    Lambda$lambda3 = 1-Lambda$lambda1-Lambda$lambda2
    if(equilateral) Lambda <- equi_transform(Lambda)
  } else if(num_metrics==4) {
    df = expand.grid(s,s,s)
    df = df[df$Var1+df$Var2+df$Var3<=1,]
    Lambda = data.frame(lambda1=df$Var1, lambda2=df$Var2, lambda3=df$Var3)
    Lambda$lambda4 = 1-Lambda$lambda1-Lambda$lambda2-Lambda$lambda3
  } else if(num_metrics==5) {
    df = expand.grid(s,s,s,s)
    df = df[df$Var1+df$Var2+df$Var3+df$Var4<=1,]
    Lambda = data.frame(lambda1=df$Var1, lambda2=df$Var2, lambda3=df$Var3, lambda4=df$Var4)
    Lambda$lambda5 = 1-Lambda$lambda1-Lambda$lambda2-Lambda$lambda3-Lambda$lambda4
  }
  
  return(Lambda)
}