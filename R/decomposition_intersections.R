#' Compute intersections from line segments for exact rank aggregation decomposition
#' 
#' Every pair of computed line segments are checked for an intersection within the
#' boundary of Lambda. 
#' Step 2 of [rank_decomposition_exact]
#'  
#' @param Linedf Data frame [decomposition_linesegs] where each row describes a line segment.
#' @param show_bar Boolean for a progress bar and time data printed in console
#' @return A data frame of two columns (x and y) for all intersection points in 
#' weight set decomposition, including those on the boundary. 
#' @export
#' @examples 
#' rankdf <- data.frame('rank1'=c(1,2,3,4,5), 
#'            'rank2'=c(2,3,1,5,4), 'rank3'=c(3,1,5,4,2))
#' Linedf = decomposition_linesegs(rankdf)
#' IntersectPts = decomposition_intersections(Linedf)
decomposition_intersections <- function(Linedf,show_bar=FALSE) {
  IntersectPts = data.frame(x=0, y=0)
  # corner points of Lambda should be included
  IntersectPts=rbind(IntersectPts,c(0,0))
  IntersectPts=rbind(IntersectPts,c(1,0))
  IntersectPts=rbind(IntersectPts,c(0,1))
  # all boundary points of line segments should be included
  for(i in 1:dim(Linedf)[1]) {
    IntersectPts=AddIfUnique(IntersectPts,c(Linedf$bp1x[i],Linedf$bp1y[i]))
    IntersectPts=AddIfUnique(IntersectPts,c(Linedf$bp2x[i],Linedf$bp2y[i]))
  }
  m=dim(Linedf)[1]
  if(show_bar) {
    estTime=max(round(0.0002*m^2/2,1),0.1)
    print(paste("Step 2: Computing intersections of ",m," line segments \n Operations to perform:",m^2/2,". Estimated time:",estTime,"seconds."))
    pb <- utils::txtProgressBar(width=20,style=3)
    tictoc::tic()
  }
  # for every pair of line segments
  for(l1 in 1:(m-1)) {
    for(l2 in 2:m) {
      l1bp1 = c(Linedf$bp1x[l1],Linedf$bp1y[l1])
      l1bp2 = c(Linedf$bp2x[l1],Linedf$bp2y[l1])
      l2bp1 = c(Linedf$bp1x[l2],Linedf$bp1y[l2])
      l2bp2 = c(Linedf$bp2x[l2],Linedf$bp2y[l2])
      # if any endpoints are equal, then skip
      if(all(l1bp1==l2bp1)) next
      if(all(l1bp1==l2bp2)) next
      if(all(l1bp2==l2bp1)) next
      if(all(l1bp2==l2bp2)) next
      # if both slopes NaN (vertical), then skip
      if(is.na(Linedf$slope[l1]) && is.na(Linedf$slope[l2])) next
      # if slopes equal, then skip
      if(Linedf$slope[l1]==Linedf$slope[l2]) next
      
      # for slope NaN (vertical), then compute intersection
      if(is.na(Linedf$slope[l1])) {
        xval = Linedf$intercept[l1]
        yval = Linedf$intercept[l2]+Linedf$slope[l2]*xval
        if(xval>=0 && yval>=0 && xval+yval<=1) IntersectPts=AddIfUnique(IntersectPts,c(xval,yval))
        next
      }
      if(is.na(Linedf$slope[l2])) {
        xval = Linedf$intercept[l2]
        yval = Linedf$intercept[l1]+Linedf$slope[l1]*xval
        if(xval>=0 && yval>=0 && xval+yval<=1) IntersectPts=AddIfUnique(IntersectPts,c(xval,yval))
        next 
      }
      # otherwise, compute intersection using formula
      xval = (Linedf$intercept[l1]-Linedf$intercept[l2])/(Linedf$slope[l2]-Linedf$slope[l1])
      yval = Linedf$intercept[l2]+Linedf$slope[l2]*xval
      if(xval>=0 && yval>=0 && xval+yval<=1) IntersectPts=AddIfUnique(IntersectPts,c(xval,yval))
      
      if(show_bar && l1%%10==0 && l2%%10==0) utils::setTxtProgressBar(pb, l1*l2/(0.5*m^2))
    }
  }
  
  if(show_bar) {
    utils::setTxtProgressBar(pb, 1)
    close(pb)
    time=tictoc::toc()
    print(paste("Total time spent in Step 2:",time[[4]]))
  }
  
  IntersectPts = IntersectPts[-1,]
  return(IntersectPts)
}