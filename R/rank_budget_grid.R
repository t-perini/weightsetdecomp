#' Budgeted ranking over the grid of weights
#' 
#' Given costs per item and a maximum budget, give the lexicographic or greedy knapsack solution per aggregate rank. 
#' The order of the items in the knapsack solution are ignored. All rank aggregations are
#' relabeled according to the items in the solution. Note that if Rank.Label is not provided in the
#' input data frame, then they will first be computed (without ties). 
#' 
#' @param Lambda Data frame containing columns lambda1, lambda2, lambda3. 
#' Runs faster if it already contains column Rank.Label from [rank_aggregation_grid] 
#' (factor/string of ordered items separated by periods)
#' @param costs Array of costs per item
#' @param budget Maximum budget which cannot be exceeded 
#' @param metrics Data frame for 3 metrics, one metric per column, one row per item, smaller values are better.
#' Only required if Rank.Label column is missing from Lambda.
#' @return An extended data frame with column for Budget, which are factors (of strings) that list 
#' the solution items (ordered numerically not by rank)
#' @export
#' @examples 
#' Lambda <- weight_set()
#' metrics <- data.frame('risk1'=c(1,2,3,4,5), 
#'            'risk2'=c(2,3,1,5,4), 'risk3'=c(3,1,5,4,2))
#' costs <- c(4.1, 4.4, 2.2, 5.5, 3.6 )
#' Lambda <- rank_budget_grid(Lambda, costs=costs, budget=10, metrics=metrics)
#' plot_aggregation_grid(Lambda,by='Budget',leg.pos='bottom')
rank_budget_grid <- function(Lambda, costs, budget, metrics) {
  Lambda$Budget = ""
  # if items have already been ranked, then use that column (faster)
  if('Rank.Label' %in% colnames(Lambda)) {
    for(i in 1:dim(Lambda)[1]) {
      # Rank.Label gives the precise rank vector of each item
      sol = lex_knapsack(Lambda$Rank.Label[i],costs,budget)
      # The item labels in the top are always in increasing order regardless of position
      Lambda$Budget[i]=paste(sol,collapse = ".")
    }
  } else {
    # otherwise, compute individually for all weights (slower)
    Lambda$Rank.Label = ""
    Lambda$Item.Label = ""
    rows=dim(Lambda)[1]
    for(i in 1:rows) {
      # for every weight (row) in Lambda compute weighted sum of metrics, then rank and store labels
      rating = Lambda$lambda1[i]*metrics[,1] + Lambda$lambda2[i]*metrics[,2] + Lambda$lambda3[i]*metrics[,3]
      # for simplicity, assuming ties are not allowed
      rr = flex_rank(rating, ties=FALSE)
      Lambda$Rank.Label[i] = paste(rr,collapse = ".")
      Lambda$Item.Label[i] = paste(order(rr),collapse = ".")
      # Rank.Label gives the precise rank vector of each item
      sol = lex_knapsack(Lambda$Rank.Label[i],costs,budget)
      # The item labels in the top are always in increasing order regardless of position
      Lambda$Budget[i]=paste(sol,collapse = ".")
    }
    # replace strings with factors
    Lambda$Rank.Label <- as.factor(Lambda$Rank.Label)
    Lambda$Item.Label <- as.factor(Lambda$Item.Label)
  }
  Lambda$Budget <- as.factor(Lambda$Budget)
  return(Lambda)
}