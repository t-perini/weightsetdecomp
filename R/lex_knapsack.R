#' Lexicographic solution to knapsack problem
#'
#' Provides the set of items for the lexicographic (or greedy) solution for a single budget constraint. 
#' Items are choosen by following the given rank vector: an item is only packed if it fits in current budget. 
#'
#' @param rank.label String describing the ranked position per item
#' @param costs Array of costs per item
#' @param budget Total budget which cannot be exceeded
#' @return A string describing the items included in the lexicographic solution in order of index
#' @export
#'
#' @examples
#' rank.label = '4.6.3.1.1.4'
#' costs <- c(1.0,  0.5,  2.1,  4.0,  4.7, 1.5)
#' lex_knapsack(rank.label,costs,budget=6)
#' lex_knapsack(rank.label,costs,budget=7)
#' lex_knapsack(rank.label,costs,budget=8)
lex_knapsack <- function(rank.label,costs,budget) {
  # Rank.Label gives the precise rank vector of each item
  parsedstring = strsplit(as.character(rank.label),'.',fixed=TRUE)
  item.order = order(as.numeric(parsedstring[[1]]))
  items <- c()
  current_cost <- 0
  for(i in item.order) {
    if(current_cost + costs[i] <= budget) {
      items <- c(items,i)
      current_cost <- current_cost + costs[i]
    }
  }
  return(sort(items))
}