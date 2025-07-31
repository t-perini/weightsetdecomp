#' Top-alpha ranking over the grid of weights
#' 
#' Alpha is the top number of positions of interest, for instance the top-5 or top-10 items in 
#' the ranked list. The order of the items in the top alpha are ignored. All rank aggregations are
#' relabeled according to their top-alpha items. Note that if Rank.Label is not provided in the
#' input data frame, then they will first be computed (without ties). 
#' 
#' @param Lambda Data frame containing columns lambda1, lambda2, lambda3. 
#' Runs faster if it already contains column Rank.Label from [rank_aggregation_grid] 
#' (factor/string of ordered items separated by periods)
#' @param alpha The number of top-ranked positions of interest. Must be at least 2. 
#' @param metrics Data frame for 3 metrics, one metric per column, one row per item, smaller values are better.
#' Only required if Rank.Label column is missing from Lambda.
#' @return An extended data frame with column for TopAlpha, which are factors (of strings) that list 
#' the top ranked items (ordered numerically not by rank)
#' @export
#' @examples 
#' Lambda <- weight_set(stepsize=0.1)
#' metrics <- data.frame('cost'=c(10,20,30,40,50), 
#'            'time'=c(5.9, 3.3, 2.5, 4.1, 1.8), 'risk'=c(1,4,3,2,5))
#' Lambda <- rank_topalpha_grid(Lambda, alpha=3, metrics=metrics)
#' plot_aggregation_grid(Lambda,by='TopAlpha')
rank_topalpha_grid <- function(Lambda, alpha=5, metrics) {
  Lambda$TopAlpha = ""
  # if items have already been ranked, then use that column (faster)
  if('Rank.Label' %in% colnames(Lambda)) {
    for(i in 1:dim(Lambda)[1]) {
      # Rank.Label gives the precise rank vector of each item
      parsedstring = strsplit(as.character(Lambda$Rank.Label[i]),'.',fixed=TRUE)
      parsed = as.numeric(parsedstring[[1]])
      # Those items with rank <= alpha are in the top
      top_alpha = which(parsed<=alpha)
      # The item labels in the top are always in increasing order regardless of position
      Lambda$TopAlpha[i]=paste(sort(top_alpha),collapse = ".")
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
      # Those items with rank <= alpha are in the top
      top_alpha = which(rr<=alpha)
      # The item labels in the top are always in increasing order regardless of position
      Lambda$TopAlpha[i]=paste(sort(top_alpha),collapse = ".")
    }
    # replace strings with factors
    Lambda$Rank.Label <- as.factor(Lambda$Rank.Label)
    Lambda$Item.Label <- as.factor(Lambda$Item.Label)
  }
  Lambda$TopAlpha <- as.factor(Lambda$TopAlpha)
  return(Lambda)
}