#' Plot the aggregations for the grid search
#' 
#' For every weight vector (row) of data frame, compute the weighted rank aggregation and generate
#' a string label. The strings are concatenations of the elements in order from best to worst, 
#' separated by periods. 
#' 
#' @param Lambda Data frame containing weight vectors and labels for weighted rank aggregations.
#' Must include column names lambda1/2 for right triangle representation.
#' @param inputlist List of 4 data frames from [rank_decomposition_exact]
#' @param triangle Specify whether the right triangle representation ("right") or the 
#' equilateral triangle representation ("equilateral"). Note that the equilateral triangle
#' representation is recommended as it is unbiased, and the right triangle representation is 
#' biased. 
#' @param with_lines Boolean for whether to add lines for Linedf
#' @param with_points Boolean for whether to add points for IntersectPts
#' @param bias_axes Boolean for whether to include bias axes in the graph
#' @param plotly_text Boolean for whether to include text which is useful for plotly conversion.
#' Note that a warning will occur about an "unknown aesthetics" (text); this is a 
#' feature which is intended to be used by the ggplotly interactive library. The warning is not
#' fixable, and should be ignored. For ggplotly visual tool, use 
#' ggplotly(g,tooltip = 'text') to access these aesthetics as appropriate labels.
#' @param leg.pos Input for ggplot legend positioning, e.g. 'bottom', 'right', or c(0.8,0.8)
#' @param random.color Seed for randomizing colors based on label (for improved readability).
#' 0 indicates no randomization.
#' @return A ggplot structure which can be plotted directly.  
#' @export
#' 
#' @examples 
#' Lambda <- weight_set()
#' rankdf <- data.frame('rank1'=c(1,2,3,4,5), 
#'            'rank2'=c(2,3,1,5,4), 'rank3'=c(3,1,5,4,2))
#' Lambda <- rank_aggregation_grid(3,Lambda,rankdf)
#' inputlist <- rank_decomposition_exact(rankdf,Lambda)
#' plot_aggregation_exact(Lambda,inputlist)
#' plot_aggregation_exact(Lambda,inputlist,
#'     with_lines=TRUE,with_points=TRUE,plotly_text=FALSE,leg.pos='bottom',random.color=0)
#' Lambda <- rank_topalpha_grid(Lambda, alpha=3, metrics=metrics)
#' inputlist <- rank_decomposition_exact(rankdf,Lambda,alpha=3)
#' plot_aggregation_exact(Lambda,inputlist,with_lines=FALSE,leg.pos='bottom')
plot_aggregation_exact <- function(Lambda,inputlist,triangle="equilateral",with_lines=TRUE,with_points=FALSE,bias_axes=TRUE,plotly_text=FALSE,leg.pos='none',random.color=1) {
  Linedf = inputlist[[1]]
  IntersectPts = inputlist[[2]]
  #IRextremepts = inputlist[[3]]
  IR_hull = inputlist[[4]]
  
  # optional: randomize order of labels for better readability
  IR_hull$Rank.Label <- randomize_labels(IR_hull$Rank.Label,seed=random.color)
  
  #Plot computed line segments and intersection points  
  g <- ggplot2::ggplot() + ggplot2::theme(legend.position=leg.pos)
  
  if(tolower(triangle)=="equilateral") {
    # transform all columns labeled lambda1, lambda2
    IR_hull <- equi_transform(IR_hull)
    colnames(IR_hull) <- c('x','y','Rank.Label','lambda1','lambda2')
    # ---
    colnames(Linedf)[which(colnames(Linedf)=='bp1x')] <- 'lambda1'
    colnames(Linedf)[which(colnames(Linedf)=='bp1y')] <- 'lambda2'
    Linedf <- equi_transform(Linedf)
    colnames(Linedf)[which(colnames(Linedf)=='lambda1')] <- 'firstlambda1'
    colnames(Linedf)[which(colnames(Linedf)=='lambda2')] <- 'firstlambda2'
    colnames(Linedf)[which(colnames(Linedf)=='equilambda1')] <- 'bp1x'
    colnames(Linedf)[which(colnames(Linedf)=='equilambda2')] <- 'bp1y'
    colnames(Linedf)[which(colnames(Linedf)=='bp2x')] <- 'lambda1'
    colnames(Linedf)[which(colnames(Linedf)=='bp2y')] <- 'lambda2'
    Linedf <- equi_transform(Linedf)
    colnames(Linedf)[which(colnames(Linedf)=='equilambda1')] <- 'bp2x'
    colnames(Linedf)[which(colnames(Linedf)=='equilambda2')] <- 'bp2y'
    # ---
    colnames(IntersectPts) <- c('lambda1','lambda2')
    IntersectPts <- equi_transform(IntersectPts)
    colnames(IntersectPts) <- c('lambda1','lambda2','x','y')
  }
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
  
  if(bias_axes && tolower(triangle)=="equilateral") {
    g <- g + ggplot2::geom_segment(ggplot2::aes(x = -0.5, y = 0, xend = 0.25, yend = 0.25*sqrt(3)), color="gray") + 
      ggplot2::geom_segment(ggplot2::aes(x = 0.5, y = 0, xend = -0.25, yend = 0.25*sqrt(3)), color="gray") + 
      ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = 0, yend = 0.5*sqrt(3)), color="gray") + 
      ggplot2::geom_segment(ggplot2::aes(x = -0.5, y = 0, xend = 0, yend = 0.5*sqrt(3)), color="black",linewidth=2) + 
      ggplot2::geom_segment(ggplot2::aes(x = -0.5, y = 0, xend = 0.5, yend = 0), color="black",linewidth=2) + 
      ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0.5*sqrt(3), xend = 0.5, yend = 0), color="black",linewidth=2) 
  } else if(bias_axes && tolower(triangle)=="right") {
    g <- g + ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = 0.5, yend = 0.5), color="gray") + 
      ggplot2::geom_segment(ggplot2::aes(x = 1, y = 0, xend = 0, yend = 0.5), color="gray") + 
      ggplot2::geom_segment(ggplot2::aes(x = 0.5, y = 0, xend = 0, yend = 1), color="gray") + 
      ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = 0, yend = 1), color="black",linewidth=2) + 
      ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = 1, yend = 0), color="black",linewidth=2) + 
      ggplot2::geom_segment(ggplot2::aes(x = 0, y = 1, xend = 1, yend = 0), color="black",linewidth=2) 
    
  }
  
  return(g)
}