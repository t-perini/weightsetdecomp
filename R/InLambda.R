#' In Lambda boolean for weight vectors
#' 
#' Utility function for exact decomposition algorithm. 
#' Returns true if the weight vector satisfies the (right-triangle) representation of weight set.
#' 2 dimensional: x + y <= 1 and x,y >= 0.
#' 3 dimensional: x + y + z == 1 and x,y,z >= 0.
#'
#' @param lambda Weight vector with two numerical components (or only the first two will be checked)
#'
#' @return True (if yes) or False (if no)
#' @export 
#'
#' @examples 
#' InLambda(c(0.5,0.5))
#' InLambda(c(0.6,0.5))
#' InLambda(c(0.5,0.5,0.5))
InLambda <- function(lambda) {
  if(any(lambda<0)) return(FALSE)
  if(length(lambda)==2) {
    if(lambda[1]+lambda[2]>1) return(FALSE)
  }
  if(length(lambda)==3) {
    if(sum(lambda)!=1) return(FALSE)
  }
  return(TRUE)
}