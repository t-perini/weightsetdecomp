#' Plot the aggregations for the grid search
#' 
#' For every weight vector (row) of data frame, compute the weighted rank aggregation and generate
#' a string label. The strings are concatenations of the elements in order from best to worst, 
#' separated by periods. 
#' 
#' @param Lambda Data frame containing weight vectors and labels for weighted rank aggregations.
#' Must include column names lambda1/2 for right triangle representation.
#' @param inputlist List of 4 data frames from [rank_decomposition_exact]
#' @param plotly_text Boolean for whether to include text which is useful for plotly conversion.
#' Note that a warning will occur about an "unknown aesthetics" (text); this is a 
#' feature which is intended to be used by the ggplotly interactive library. The warning is not
#' fixable, and should be ignored. For ggplotly visual tool, use 
#' ggplotly(g,tooltip = 'text') to access these aesthetics as appropriate labels.
#' @param with_lines Boolean for whether to add lines for Linedf
#' @param with_points Boolean for whether to add points for IntersectPts
#' @param leg.pos Input for ggplot legend positioning, e.g. 'bottom', 'right', or c(0.8,0.8)
#' @return A ggplot structure which can be plotted directly.  
#' @export
#' 
#' @examples 
#' Lambda <- weight_set(0.1)
#' rankdf <- data.frame('rank1'=c(1,2,3,4,5), 
#'            'rank2'=c(2,3,1,5,4), 'rank3'=c(3,1,5,4,2))
#' Lambda <- Lambda <- rank_aggregation_grid(Lambda,rankdf)
#' inputlist <- rank_decomposition_exact(rankdf,Lambda)
#' plot_aggregation_exact(Lambda,inputlist)
#' plot_aggregation_exact(Lambda,inputlist,
#'     with_lines=TRUE,with_points=TRUE,plotly_text=FALSE,leg.pos='bottom')
plot_aggregation_exact <- function(Lambda,inputlist,with_lines=TRUE,with_points=FALSE,plotly_text=FALSE,leg.pos='none') {
  Linedf = inputlist[[1]]
  IntersectPts = inputlist[[2]]
  #IRextremepts = inputlist[[3]]
  IR_hull = inputlist[[4]]
  
  #Plot computed line segments and intersection points  
  g <- ggplot2::ggplot() + ggplot2::theme(legend.position=leg.pos)
  
  if(plotly_text) {
    g <- g + ggplot2::geom_polygon(data = IR_hull, 
                                   ggplot2::aes(x=lambda1, y=lambda2, fill=Rank.Label,
                                        text=paste("Rank:",Rank.Label))) 
  } else {
    g <- g + ggplot2::geom_polygon(data = IR_hull, 
                                   ggplot2::aes(x=lambda1, y=lambda2, fill=Rank.Label))
  }
    
  
  if(with_lines) {
    g <- g+
      #line segments: 
      ggplot2::geom_segment(data=Linedf,ggplot2::aes(x=bp1x,y=bp1y,xend=bp2x,yend=bp2y,group=label)) 
  }
  if(with_points) {
    g <- g+
      #intersection points: 
      ggplot2::geom_point(data=IntersectPts, ggplot2::aes(x=x, y=y)) 
  }
  
  
  return(g)
}