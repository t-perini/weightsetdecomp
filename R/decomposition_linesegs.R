#' Compute line segments for exact rank aggregation decomposition
#' 
#' Every pair of items is used to compute a line. The intersections of the line with
#' boundary of Lambda (if there are any) are called boundary points.  
#' Computes all boundary points and returns. 
#' Step 1 of [rank_decomposition_exact]
#'  
#' @param input_data Data frame containing 3 columns of ranks/ratings per item. 
#' Each row represents one item. 
#' @param show_bar Boolean for a progress bar and time data printed in console
#' @return A data frame of labeled line segments contained within Lambda.
#' Lines are labeled by two boundary points given by x & y: (bp1x, bp1y) and (bp2x, bp2y).
#' The slope and y-intercept are given in two columns, as well as the item ids 
#' used to compute the line. 
#' @export
#' @examples 
#' rankdf <- data.frame('rank1'=c(1,2,3,4,5), 
#'            'rank2'=c(2,3,1,5,4), 'rank3'=c(3,1,5,4,2))
#' Linedf = decomposition_linesegs(rankdf)
#' Linedf = decomposition_linesegs(rankdf, show_bar=TRUE)
decomposition_linesegs <- function(input_data,show_bar=FALSE) {
  #describe lines by border points (bp), i.e., where they intersect with border of triangle
  Linedf = data.frame(bp1x = 0, bp1y = 0, bp2x = 0, bp2y = 0, slope = 0, intercept = 0,
                      itemA = 0,itemB = 0, label="blank")
  n=dim(input_data)[1]
  rank1=input_data[,1]
  rank2=input_data[,2]
  rank3=input_data[,3]
  
  if(show_bar) {
    estTime=max(round(0.00178*n^2/2,1),0.1)
    print(paste("Step 1: Computing ednpoints of line segments \n Operations to perform:",n^2/2,". Estimated time:",estTime,"seconds."))
    pb <- utils::txtProgressBar(width=20,style=3)
    tictoc::tic()
  }
  
  # for every pair of items
  for(A in 1:(n-1)) {
    for(B in (A+1):n){
      # first, check if all ranks agree, and if so then skip this pair
      AbetterthanB=0
      if(rank1[A]<=rank1[B]) AbetterthanB=AbetterthanB+1
      if(rank2[A]<=rank2[B]) AbetterthanB=AbetterthanB+1
      if(rank3[A]<=rank3[B]) AbetterthanB=AbetterthanB+1
      if(AbetterthanB==0 || AbetterthanB==3) next
      
      # so there is disparity on which is best, continue computation
      delta1=rank1[A]-rank1[B]
      delta2=rank2[A]-rank2[B]
      delta3=rank3[A]-rank3[B]
      if(delta2-delta3==0) { # vertical line case with NaN slope
        val = -delta3/(delta1-delta3)
        slope=NaN
        incpt=val
        bp1x = val
        bp1y = 0
        bp2x = val
        bp2y = 1-val
        
      } else { # diagonal line with formula for slope
        slope=-(delta1-delta3)/(delta2-delta3)
        incpt=-delta3/(delta2-delta3)
        #3 dummy points. the one that is not replaced will not be returned
        p1=c(-1,-1) 
        p2=c(-1,-1)
        p3=c(-1,-1)
        if(delta1!=delta3) {
          # intersection with y=0
          p1=c(-delta3/(delta1-delta3), 0)
        } 
        if(delta2!=delta3) {
          # intersection with x=0
          p2=c(0,-delta3/(delta2-delta3))
        }
        if(delta1!=delta2) {
          # intersection with x+y=1
          p3[1]=-delta2/(delta1-delta2)
          p3[2]=1-p3[1]
        }
        # if one of p1/p2/p3 is outside of Lambda, 
        # then just describe line with the two that are inside
        if(InLambda(p1)==FALSE) {
          bp1x = p2[1]
          bp1y = p2[2]
          bp2x = p3[1]
          bp2y = p3[2]  
        } else if(InLambda(p2)==FALSE) {
          bp1x = p1[1]
          bp1y = p1[2]
          bp2x = p3[1]
          bp2y = p3[2]  
        } else if(InLambda(p3)==FALSE) {
          bp1x = p1[1]
          bp1y = p1[2]
          bp2x = p2[1]
          bp2y = p2[2]  
        } else { 
          #if all are in Lambda, then two should be equivalent (at a vertex)
          # describe the line with two distinct points
          if(all(p1==p2)) {
            bp1x = p2[1]
            bp1y = p2[2]
            bp2x = p3[1]
            bp2y = p3[2]  
          } else if(all(p2==p3)) {
            bp1x = p1[1]
            bp1y = p1[2]
            bp2x = p3[1]
            bp2y = p3[2]  
          } else if(all(p1==p3)) {
            bp1x = p1[1]
            bp1y = p1[2]
            bp2x = p2[1]
            bp2y = p2[2]  
          }
        }
      }
      newrow = data.frame(bp1x = bp1x, bp1y = bp1y, bp2x = bp2x, bp2y = bp2y, 
                          slope = slope, intercept = incpt,
                          itemA = A,itemB = B, label=paste(c(A,B),collapse = "&"))
      Linedf = rbind(Linedf,newrow)
      
      if(show_bar && A%%10==0 && B%%10==0) utils::setTxtProgressBar(pb, A*B/(0.5*n^2))
    }
  }
  
  if(show_bar) {
    utils::setTxtProgressBar(pb, 1)
    close(pb)
    time=tictoc::toc()
    print(paste("Total time spent in Step 1:",time[[4]]))
  }
  # remove starter row
  Linedf <- Linedf[-1,]
  Linedf$label<-as.factor(Linedf$label)
  return(Linedf)
}
  
  