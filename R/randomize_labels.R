#' Randomize the order of levels of a factor for better color separation in graphs
#' 
#' Randomly reorders the levels of some factor's labels. This helps when graphing because neighboring
#' indifference regions tend to have the same color. By randomizing the order, the colors are
#' more randomized and distinguishable. Recommended use for Lambda$Rank.Label or Lambda$TopAlpha.
#' 
#' @param label Column or array of labels, e.g., Rank.Label or Item.Label
#' @param seed 0 to indicate no randomization, otherwise the seed for randomization
#' @return Array of same labels but as a factor with the levels randomly shuffled
#' @export
#' 
#' @examples 
#' Lambda <- weight_set()
#' metrics <- data.frame('cost'=c(10,20,30,40), 'time'=c(5.9, 3.3, 2.5, 4.1), 'risk'=c(1,4,3,2))
#' Lambda <- rank_aggregation_grid(3,Lambda,metrics)
#' Lambda$Rank.Label <- randomize_labels(Lambda$Rank.Label)
randomize_labels <- function(label,seed=0) {
  label <- as.factor(label)
  if(seed==0) {
    return(label)
  } else {
    #randomize order of levels
    set.seed(seed)
    ordered_levels <- levels(label)
    label <- factor(label, levels=sample(ordered_levels))
    return(label)
  }
}