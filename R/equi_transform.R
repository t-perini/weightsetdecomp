#' Transform weights to the equilateral triangle representation
#' 
#' Appends columns to the data frame from [weight_set()] which transforms the right triangle representation 
#' (lambda1 and lambda2) to the equilateral triangle representation (equilambda1 and equilambda2). 
#' Note equilambda1 is centered at 0, so it is expected to have negative values. 
#' These weights are recommended for less biased visualizations.  
#' 
#' @param Lamdba Data frame with column names lambda1 and lambda2, representing weights for the
#' right triangle representation 
#' @return An extended data frame with extra columns named equilambda1 and equilambda2
#' @export 
#' 
#' @examples 
#' Lambda1 <- weight_set(0.01)
#' Lambda2 <- equi_transform(Lambda1)
equi_transform <- function(Lambda) {
  Lambda$equilambda1 <- Lambda$lambda1-0.5*(1-Lambda$lambda2)
  Lambda$equilambda2 <- 0.5*sqrt(3)*Lambda$lambda2
  return(Lambda)
}