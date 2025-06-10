#' Appending functions for accumulating weight vectors
#' 
#' Utility function for exact decomposition algorithm. 
#' Append the row to data frame only if it does not already exist within it. 
#'
#' @param lambda Weight vector with two numerical components (or only the first two will be checked)
#'
#' @return The data frame, possibly with appended new row
#' @export 
#'
#' @examples 
#' Lambda <- weight_set(0.5)
#' weight <- c(0.1,0,0.9, -0.4, 0)
#' Lambda <- AddIfUnique(Lambda, weight)
AddIfUnique <- function(df,row) {
  found=FALSE
  for(i in 1:dim(df)[1]) {
    if(all(df[i,]==row)) {
      found=TRUE
      break
    }
  }
  if(!found) df=rbind(df,row)
  return(df)
}