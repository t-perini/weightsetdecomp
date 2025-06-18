#' Exact decomposition algorithm for rank aggregations
#' 
#' Compute data frames which represent the exact decomposition of weight set 
#' into all indifference regions representing all possible weighted rank aggregations. 
#' The process requires multiple steps, including: 
#' 1. computing line segments that bisect the weight set 
#' 2. computing intersections among these line segments (and the boundaries of weight set)
#' 3. labeling all valid ranks for these intersections
#' 4. computing the convex hull of every aggregate rank's indifference region. 
#' All four data frames will be returned.
#' 
#' @param input_data Data frame containing 3 columns of ranks/ratings per item. 
#' Each row represents one item. 
#' @param Lambda 
#' @param alpha The value for alpha if computing top-alpha regions
#' @param show_bar Boolean for a progress bar and time data printed in console
#' @return An extended data frame with column for TopAlpha, which are factors (of strings) that list 
#' the top ranked items (ordered numerically not by rank)
#' @importFrom magrittr %>%
#' @export
#' @examples 
#' Lambda <- weight_set(0.1)
#' rankdf <- data.frame('rank1'=c(1,2,3,4,5), 
#'            'rank2'=c(2,3,1,5,4), 'rank3'=c(3,1,5,4,2))
#' Lambda <- rank_aggregation_grid(Lambda,rankdf)
#' inputlist <- rank_decomposition_exact(rankdf,Lambda)
#' Lambda <- rank_topalpha_grid(Lambda,alpha=3,rankdf)
#' inputlist <- rank_decomposition_exact(rankdf,Lambda,alpha=3)
rank_decomposition_exact <- function(input_data,Lambda,alpha=NaN,show_bar=FALSE) {
  # Step 1: Compare items pair-wise to compute line segments
  Linedf = decomposition_linesegs(rankdf,show_bar)
  # Step 2: Compare line segments pair-wise to compute intersections
  IntersectPts = decomposition_intersections(Linedf,show_bar)
  # Step 3: Label intersection points by rank labels to be extreme points
  IRextremepts = decomposition_assignranks(rankdf,IntersectPts,Lambda,alpha=alpha,show_bar)
  # Step 4: Construct the convex hull for each indifference region
  IR_hull <- IRextremepts %>%
    dplyr::group_by(Rank.Label) %>%
    dplyr::slice(grDevices::chull(lambda1, lambda2))
  
  return(list(Linedf,IntersectPts,IRextremepts,IR_hull))
}
