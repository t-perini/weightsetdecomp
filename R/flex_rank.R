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
#' run_times <- c(5.3, 6.8, 2.1, 2.1, 3.4)
#' flex_rank(run_times)
#' flex_rank(run_times,ties=TRUE)
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
