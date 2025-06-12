#' Flexible ranking function
#'
#' Provides two ways of ranking input vector of scores/ratings. The default method does not
#' allow for ties, so in the case of a tie, it is broken by order of indices. The alternative
#' method allows for ties, and it is computed by the number of smaller values.
#'
#' @param vec Vector of scores/ratings to be ranked where smaller values are better
#' @param ties Boolean to indicate whether the ranking should allow for ties (TRUE) or not (FALSE)
#' @return A vector of the same length in which the ith value is the rank of the ith item
#' @export
#'
#' @examples
#' risk <- c(1.0,  2.0,  3.1,  2.0,  1.7)
#' flex_rank(risk)
#' flex_rank(risk,ties=TRUE)
flex_rank <- function(vec,ties=FALSE) {
  if(ties==FALSE) {
    return(rank(vec, ties.method="first"))
  } else {
    rank <- c()
    for(i in 1:length(vec)) {
      rank = c(rank,1+sum(vec< vec[i]))
    }
    return(rank)
  }
}
