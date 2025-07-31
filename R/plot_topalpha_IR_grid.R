#' Plot one top-alpha region for the grid search
#' 
#' Input either a top-alpha string (items separated by periods) or a weight vector.
#' The top-alpha region for the given ranked order (or containing the weight vector) 
#' will be plotted in the weight set. 
#' 
#' @param Lambda Data frame containing weight vectors and labels for weighted rank aggregations.
#' Must include column names lambda1/2 for right triangle representation or 
#' equilambda1/2 for equilateral triangle representation. 
#' Must include column name Label for the string/factor identifier of the rank aggregation.
#' @param weight Weight vector identifying the indifference region to plot. 
#' @param top.alpha The string identifying the top-alpha items. 
#' This should be a string with the indices separated by periods. 
#' @param triangle Specify whether the right triangle representation ("right") or the 
#' equilateral triangle representation ("equilateral"). Note that the equilateral triangle
#' representation is recommended as it is unbiased, and the right triangle representation is 
#' biased. 
#' @param bias_axes Boolean for whether to include bias axes in the graph
#' @param annotations Boolean for whether to include annotations in the graph
#' @param plotly_text Boolean for whether to include text which is useful for plotly conversion.
#' Note that a warning will occur about an "unknown aesthetics" (text); this is a 
#' feature which is intended to be used by the ggplotly interactive library. The warning is not
#' fixable, and should be ignored. For ggplotly visual tool, use 
#' ggplotly(g,tooltip = 'text') to access these aesthetics as appropriate labels.
#' @return A ggplot structure which can be plotted directly.  
#' @export
#' 
#' @examples 
#' Lambda <- weight_set()
#' metrics <- data.frame('cost'=c(10,20,30,40,50), 
#'            'time'=c(5.9, 3.3, 2.5, 4.1, 1.8), 'risk'=c(1,4,3,2,5))
#' Lambda <- rank_topalpha_grid(Lambda, alpha=3, metrics=metrics)
#' plot_topalpha_IR_grid(Lambda,weight=c(0.33,0.33,0.33),triangle='right')
#' plot_topalpha_IR_grid(Lambda,top.alpha='1.3.4')
plot_topalpha_IR_grid <- function(Lambda,weight=0,top.alpha="",triangle="equilateral",bias_axes=TRUE,annotations=TRUE,plotly_text=FALSE) {
  # initial ggplot structure with theme
  g <- ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::theme(legend.position='none') 
  # check for missing input params
  if(length(weight)+nchar(top.alpha)==0) {
    print("No selection criteria input. Include either weight or top.alpha")
    return(g)
  }
  # if a weight vector is given, find the nearest weight to filter Lambda df
  if(length(weight)>0) {
    dist_df = data.frame(d1=(Lambda$lambda1-weight[1])^2,
                         d2=(Lambda$lambda2-weight[2])^2,
                         d3=(Lambda$lambda3-weight[3])^2)
    dist_df$dist=dist_df$d1+dist_df$d2+dist_df$d3
    ind = which(dist_df$dist==min(dist_df$dist))
    subdf <- subset(Lambda,TopAlpha==Lambda$TopAlpha[ind])
  }
  # if a rank vector is given, then use it to filter Lambda df
  if(nchar(top.alpha)>0) {
    if(!any(Lambda$TopAlpha==top.alpha)) {
      print("top.alpha is not found in Lambda$TopAlpha")
      return(g)
    }
    subdf <- subset(Lambda,TopAlpha==top.alpha)
  }
  # Option 1: Equilateral transformation
  if(tolower(triangle)=="equilateral") {
    if(plotly_text) { # optional plotly text as part of aes()
      g <- g + 
        ggplot2::geom_point(data=subdf, ggplot2::aes(x=equilambda1,y=equilambda2,color=TopAlpha,
                                                     text=paste("lambda:",round(equilambda1,2),round(equilambda2,2),"\nrank:",TopAlpha))) 
    } else {
      g <- g + 
        ggplot2::geom_point(data=subdf, ggplot2::aes(x=equilambda1,y=equilambda2,color=TopAlpha)) 
    }
    # optional gray lines
    if(bias_axes) {
      g <- g + ggplot2::geom_segment(ggplot2::aes(x = -0.5, y = 0, xend = 0.25, yend = 0.25*sqrt(3)), color="gray") + 
        ggplot2::geom_segment(ggplot2::aes(x = 0.5, y = 0, xend = -0.25, yend = 0.25*sqrt(3)), color="gray") + 
        ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = 0, yend = 0.5*sqrt(3)), color="gray") + 
        ggplot2::geom_segment(ggplot2::aes(x = -0.5, y = 0, xend = 0, yend = 0.5*sqrt(3)), color="black",linewidth=2) + 
        ggplot2::geom_segment(ggplot2::aes(x = -0.5, y = 0, xend = 0.5, yend = 0), color="black",linewidth=2) + 
        ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0.5*sqrt(3), xend = 0.5, yend = 0), color="black",linewidth=2) 
    }
    # optional axis and other labels
    if(annotations) {
      labels = data.frame(x=c(0.5,0,-0.5),
                          y=c(0,0.5*sqrt(3),0),
                          deltay=c(-0.05,0.05,-0.05),
                          lab=c("r1", "r2", "r3"))
      pct=round(100*dim(subdf)[1]/dim(Lambda)[1],2)
      g <- g + 
        ggplot2::geom_point(data=labels, ggplot2::aes(x=x, y=y),size=3) +
        ggplot2::geom_text(data=labels, ggplot2::aes(x=x, y=y+deltay, label=lab))+
        ggplot2::geom_label(ggplot2::aes(x=-0.5,y=0.5,label=paste("Area (%): ",pct))) +
        ggplot2::xlim(c(-0.6,0.6)) + 
        ggplot2::ylim(c(-0.05,0.95))
    }
  }
  # Option 2: Right triangle
  if(tolower(triangle)=="right") {
    if(plotly_text) { # optional plotly text as part of aes()
      g <- g + 
        ggplot2::geom_point(data=subdf, ggplot2::aes(x=lambda1,y=lambda2,color=TopAlpha,
                                                     text=paste("lambda:",round(equilambda1,2),round(equilambda2,2),"\nrank:",TopAlpha))) 
    } else {
      g <- g + 
        ggplot2::geom_point(data=subdf, ggplot2::aes(x=lambda1,y=lambda2,color=TopAlpha)) 
    }  
    # optional gray lines
    if(bias_axes) {
      g <- g + ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = 0.5, yend = 0.5), color="gray") + 
        ggplot2::geom_segment(ggplot2::aes(x = 1, y = 0, xend = 0, yend = 0.5), color="gray") + 
        ggplot2::geom_segment(ggplot2::aes(x = 0.5, y = 0, xend = 0, yend = 1), color="gray") + 
        ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = 0, yend = 1), color="black",linewidth=2) + 
        ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = 1, yend = 0), color="black",linewidth=2) + 
        ggplot2::geom_segment(ggplot2::aes(x = 0, y = 1, xend = 1, yend = 0), color="black",linewidth=2) 
    }
    # optional axis and other labels
    if(annotations) {
      labels = data.frame(x=c(1,0,0),
                          y=c(0,1,0),
                          deltay=c(-0.05,0.05,-0.05),
                          lab=c("r1", "r2", "r3"))
      pct=round(100*dim(subdf)[1]/dim(Lambda)[1],2)
      g <- g + 
        ggplot2::geom_point(data=labels, ggplot2::aes(x=x, y=y),size=3) +
        ggplot2::geom_text(data=labels, ggplot2::aes(x=x, y=y+deltay, label=lab))+
        ggplot2::geom_label(ggplot2::aes(x=0.75,y=0.75,label=paste("Area (%): ",pct))) +
        ggplot2::xlim(c(-0.1,1.1)) + 
        ggplot2::ylim(c(-0.05,1.05))
    }
  }
  return(g)
}