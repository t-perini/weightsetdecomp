#' Determine if two rankings are adjacent in the decomposition
#'
#' Uses tests for adjacency to determine if rank aggregations have adjacent indifference regions. 
#' The 'basic' test uses Rank.Label and returns true if all pairs of transpositions are adjacent and degenerate.  
#'
#' @param label1 String for the first ranked list
#' @param label2 String for the second ranked list
#' @param metrics Data frame for 3 metrics, one metric per column, one row per item, smaller values are better.
#' @param test String to label the type of test to check: 'basic' or...
#' @return TRUE or FALSE based on tests
#' @export
#'
#' @examples
#' metrics <- data.frame('risk1'=c(1,2,3,4,5), 
#'            'risk2'=c(2,3,1,5,4), 'risk3'=c(3,1,5,4,2))
#' are_adjacent('1.2.3.4.5', '1.3.2.4.5', metrics)
#' are_adjacent('1.2.3.4.5', '1.3.2.5.4', metrics)
#' metrics <- data.frame('risk1'=c(1,2,3,4,5), 
#'            'risk2'=c(1,2,3,4,5), 'risk3'=c(1,3,2,5,4))
#' are_adjacent('1.2.3.4.5', '1.3.2.5.4', metrics)
are_adjacent <- function(label1,label2,metrics,test='basic') {
  if(test=='basic') {
    # transrom Rank.Label string into list of numeric
    parsed1 = as.numeric(unlist(strsplit(as.character(label1),'.',fixed=TRUE)))
    parsed2 = as.numeric(unlist(strsplit(as.character(label2),'.',fixed=TRUE)))
    # identify number and indices of differences between ranks
    num_differences = sum(parsed1 != parsed2)
    where_different = which(parsed1 != parsed2)
    if(num_differences==2 && abs(parsed1[where_different[1]]-parsed1[where_different[2]])==1) {
      # if there are only two differences, then the ranked positions must differ by exactly one to be adjacent
      return(TRUE)
    } else if(num_differences==4) {
      # for more differences, they must come in natural pairs that differ by exactly one to be adjacent
      positions = sort(parsed1[where_different])
      position_pair1 = positions[1:2]
      position_pair2 = positions[3:4]
      if(abs(position_pair1[1]-position_pair1[2])>1 || abs(position_pair2[1]-position_pair2[2])>1) 
        return(FALSE)
      # finally, check if the two pairs are degenerate, meaning they generate the same line/hyperplane
      # all the following ratios should equal the same constant
      item_pair1 = c(which(parsed1==position_pair1[1]),which(parsed1==position_pair1[2]))
      item_pair2 = c(which(parsed1==position_pair2[1]),which(parsed1==position_pair2[2]))
      constant = (metrics[item_pair1[1],1] - metrics[item_pair1[2],1])/(metrics[item_pair2[1],1] - metrics[item_pair2[2],1])
      if(constant != (metrics[item_pair1[1],2] - metrics[item_pair1[2],2])/(metrics[item_pair2[1],2] - metrics[item_pair2[2],2])) 
        return(FALSE)
      if(constant != (metrics[item_pair1[1],3] - metrics[item_pair1[2],3])/(metrics[item_pair2[1],3] - metrics[item_pair2[2],3])) 
        return(FALSE)
      # passed basic test!
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  return(FALSE)
}