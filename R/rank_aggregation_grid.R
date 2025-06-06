#' Weighted rank aggregation via uniform grid search over weight set
#' 
#' For every weight vector (row) of data frame, compute the weighted rank aggregation and generate
#' a string label. The strings are concatenations of the elements in order from best to worst, 
#' separated by periods. Note that grid search is not guaranteed to capture every possible weighted 
#' rank aggregation; the density of the mesh grid determines the number of indifference regions 
#' discovered. Smaller step size of the grid leads to capturing more indifference regions at
#' the cost of additional computations. 
#' 
#' @param Lambda Data frame containing weight vectors with column names lambda1, lambda2, lambda3
#' @param metrics Data frame for 3 metrics, one metric per column, one row per item, smaller values are better
#' @param ties Boolean for whether to allow for ties (TRUE) or not (FALSE)
#' @param show_bar Boolean for a progress bar printed in console
#' @return An extended data frame with new columns Label and ItemLabel. 
#' Rank.Label is a string (factor, separated by periods) that lists the ranked position per item in 
#' the order of items. This is the precise description of the aggregated rank, but not very interpretable. 
#' Item.Label gives the order of items ranked from best-to-last. This is easier to interpret than Rank.Label.
#' @export
#' @examples 
#' Lambda <- weight_set(0.1)
#' metrics <- data.frame('cost'=c(10,20,30,40), 'time'=c(5.9, 3.3, 2.5, 4.1), 'risk'=c(1,4,3,2))
#' Lambda <- rank_aggregation_grid(Lambda,metrics)
rank_aggregation_grid <- function(Lambda,metrics,ties=FALSE,show_bar=FALSE) {
  rows=dim(Lambda)[1]
  if(show_bar) {# optional progress bar using utils
    print(paste("This may take some time. Weights to compute:",rows,". Estimated time:",round(0.00038*rows,1),"seconds."))
    pb <- utils::txtProgressBar(width=20,style=3)
  } 
  Lambda$Rank.Label = ""
  Lambda$Item.Label = ""
  for(i in 1:rows) {
    # for every weight (row) in Lambda compute weighted sum of metrics, then rank and store labels
    rating = Lambda$lambda1[i]*metrics[,1] + Lambda$lambda2[i]*metrics[,2] + Lambda$lambda3[i]*metrics[,3]
    rr = flex_rank(rating, ties)
    Lambda$Rank.Label[i] = paste(rr,collapse = ".")
    Lambda$Item.Label[i] = paste(order(rr),collapse = ".")
    if(i%%50==0 && show_bar) utils::setTxtProgressBar(pb, i/rows)
  }
  if(show_bar) {
    utils::setTxtProgressBar(pb, 1)
    close(pb)
  }
  # replace strings with factors
  Lambda$Rank.Label <- as.factor(Lambda$Rank.Label)
  Lambda$Item.Label <- as.factor(Lambda$Item.Label)
  return(Lambda)
}