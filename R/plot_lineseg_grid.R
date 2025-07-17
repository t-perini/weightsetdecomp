#' Plot comparison of two items with respec to the grid search
#' 
#' Input a pair of indices as a list, e.g., c(1,2).
#' The line segment separating this adjacent transposition will be plotted. 
#' The data from the grid search will be colored to compare whether the two agree.
#' 
#' @param Lambda Data frame containing weight vectors and labels for weighted rank aggregations.
#' Must include column names lambda1/2 for right triangle representation or 
#' equilambda1/2 for equilateral triangle representation.
#' @param Linedf A data frame containing information of line segments, computed from 
#' [rank_decomposition_exact()].
#' @param item_pair A pair of indices using c(A,B) notation
#' @param triangle Specify whether the right triangle representation ("right") or the 
#' equilateral triangle representation ("equilateral"). Note that the equilateral triangle
#' representation is recommended as it is unbiased, and the right triangle representation is 
#' biased. 
#' @param annotations Boolean for whether to include annotations in the graph
#' @param leg.pos Input for ggplot legend positioning, e.g. 'bottom', 'right', or c(0.8,0.8)
#' @return A ggplot structure which can be plotted directly.  
#' @export
#' 
#' @examples 
#' Lambda <- weight_set(0.01)
#' rankdf <- data.frame('rank1'=c(1,2,3,4,5), 
#'            'rank2'=c(2,3,1,5,4), 'rank3'=c(3,1,5,4,2))
#' Lambda <- rank_aggregation_grid(Lambda,rankdf)
#' plot_aggregation_grid(Lambda)
#' outlist <- rank_decomposition_exact(metrics,Lambda)
#' Linedf <- outlist[[1]]
#' g <- plot_lineseg_grid(Lambda,Linedf,item_pair=c(1,2))
#' g
#' plot_lineseg_grid(Lambda,Linedf,item_pair=c(1,3),annotations=TRUE)
#' g <- plot_aggregation_grid(Lambda,plotly_text = TRUE)
#' plotly::ggplotly(g, tooltip='text')
plot_lineseg_grid <- function(Lambda,Linedf,item_pair=c(1,2),triangle="right",annotations=FALSE,leg.pos='none') {
  # initial ggplot structure with theme
  g <- ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::theme(legend.position=leg.pos) 
  
  A=min(item_pair)
  B=max(item_pair)
  ind=which(Linedf$itemA==A & Linedf$itemB==B)
  if(length(ind)==0) {
    print("Item pair not found in Linedf")
    return(g)
  } else {
    print("Computed info for separating line:")
    print(Linedf[ind,])
  }
  
  Lambda$AbetterthanB=FALSE
  Lambda$Arank=0
  Lambda$Brank=0
  for(i in 1:dim(Lambda)[1]) {
    strlist = strsplit(as.character(Lambda$Rank.Label[i]),".",fixed=TRUE)
    ranking_vec = as.numeric(strlist[[1]])
    Lambda$Arank[i]=ranking_vec[A]
    Lambda$Brank[i]=ranking_vec[B]
    if(ranking_vec[A]<ranking_vec[B]) Lambda$AbetterthanB[i]=TRUE
  }
  
  # Option 1: Right triangle
  if(tolower(triangle)=="right") {
    # plot grid points colored by which item is better
    g <- g + 
        ggplot2::geom_point(data=Lambda, ggplot2::aes(x=lambda1,y=lambda2,color=AbetterthanB)) 
    
    # add the separating line segment from Linedf
    g <- g + ggplot2::geom_segment(data=Linedf[ind,],
                                   ggplot2::aes(x=bp1x,y=bp1y,xend=bp2x,yend=bp2y,group=label)) 
    
    # optional axis and other labels
    if(annotations) {
      # percentage of weights for extra labels
      pct_df <- data.frame(x=c(mean(Lambda$lambda1[which(Lambda$AbetterthanB)]),
                               mean(Lambda$lambda1[which(!Lambda$AbetterthanB)])),
                           y=c(mean(Lambda$lambda2[which(Lambda$AbetterthanB)]),
                               mean(Lambda$lambda2[which(!Lambda$AbetterthanB)])),
                           pct=c(round(100*sum(Lambda$AbetterthanB)/dim(Lambda)[1],2),
                                 round(100*sum(!Lambda$AbetterthanB)/dim(Lambda)[1],2)),
                           item=c(A,B)
      )
      # standard labels
      labels = data.frame(x=c(1,0,0),
                          y=c(0,1,0),
                          deltay=c(-0.05,0.05,-0.05),
                          lab=c("r1", "r2", "r3"))
      g <- g + 
        ggplot2::geom_point(data=labels, ggplot2::aes(x=x, y=y),size=3) +
        ggplot2::geom_text(data=labels, ggplot2::aes(x=x, y=y+deltay, label=lab))+
        ggplot2::geom_text(data=pct_df,ggplot2::aes(x=x,y=y,label=paste(pct,"%\n",item,' better')),hjust=0.5) +
        ggplot2::xlim(c(-0.1,1.1)) + 
        ggplot2::ylim(c(-0.05,1.05))
    }
    # add standard outline
    g <- g+ ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = 0, yend = 1), color="black",linewidth=2) + 
            ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = 1, yend = 0), color="black",linewidth=2) + 
            ggplot2::geom_segment(ggplot2::aes(x = 0, y = 1, xend = 1, yend = 0), color="black",linewidth=2) 
  } else {
    # Option 2: Equilateral triangle
    # plot grid points colored by which item is better
    g <- g + 
      ggplot2::geom_point(data=Lambda, ggplot2::aes(x=lambda1,y=lambda2,color=AbetterthanB)) 
    
    # add the separating line segment from Linedf
    g <- g + ggplot2::geom_segment(data=Linedf[ind,],
                                   ggplot2::aes(x=bp1x,y=bp1y,xend=bp2x,yend=bp2y,group=label)) 
    
    # optional axis and other labels
    if(annotations) {
      # percentage of weights for extra labels
      pct_df <- data.frame(x=c(mean(Lambda$lambda1[which(Lambda$AbetterthanB)]),
                               mean(Lambda$lambda1[which(!Lambda$AbetterthanB)])),
                           y=c(mean(Lambda$lambda2[which(Lambda$AbetterthanB)]),
                               mean(Lambda$lambda2[which(!Lambda$AbetterthanB)])),
                           pct=c(round(100*sum(Lambda$AbetterthanB)/dim(Lambda)[1],2),
                                 round(100*sum(!Lambda$AbetterthanB)/dim(Lambda)[1],2)),
                           item=c(A,B)
      )
      # standard labels
      labels = data.frame(x=c(1,0,0),
                          y=c(0,1,0),
                          deltay=c(-0.05,0.05,-0.05),
                          lab=c("r1", "r2", "r3"))
      g <- g + 
        ggplot2::geom_point(data=labels, ggplot2::aes(x=x, y=y),size=3) +
        ggplot2::geom_text(data=labels, ggplot2::aes(x=x, y=y+deltay, label=lab))+
        ggplot2::geom_text(data=pct_df,ggplot2::aes(x=x,y=y,label=paste(pct,"%\n",item,' better')),hjust=0.5) +
        ggplot2::xlim(c(-0.1,1.1)) + 
        ggplot2::ylim(c(-0.05,1.05))
    }
    # add standard outline
    g <- g+ ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = 0, yend = 1), color="black",linewidth=2) + 
      ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = 1, yend = 0), color="black",linewidth=2) + 
      ggplot2::geom_segment(ggplot2::aes(x = 0, y = 1, xend = 1, yend = 0), color="black",linewidth=2) 
  }
  
  
  return(g)
}