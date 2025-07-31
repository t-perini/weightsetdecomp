#' Plot the aggregations for the grid search
#' 
#' For every weight vector (row) of data frame, compute the weighted rank aggregation and generate
#' a string label. The strings are concatenations of the elements in order from best to worst, 
#' separated by periods. 
#' 
#' @param Lambda Data frame containing weight vectors and labels for weighted rank aggregations.
#' Must include column names lambda1/2 for right triangle representation or 
#' equilambda1/2 for equilateral triangle representation.
#' @param by Column name for the string/factor identifier of the rank aggregation. 
#' Most common: Rank.Label, Item.Label, TopAlpha, or Budget
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
#' @param leg.pos Input for ggplot legend positioning, e.g. 'bottom', 'right', or c(0.8,0.8)
#' @param random.color Seed for randomizing colors based on label (for improved readability).
#' 0 indicates no randomization.
#' @return A ggplot structure which can be plotted directly.  
#' @export
#' 
#' @examples 
#' Lambda <- weight_set()
#' metrics <- data.frame('cost'=c(10,20,30,40), 'time'=c(5.9, 3.3, 2.5, 4.1), 'risk'=c(1,4,3,2))
#' Lambda <- rank_aggregation_grid(3,Lambda,metrics)
#' plot_aggregation_grid(Lambda)
#' g <- plot_aggregation_grid(Lambda,by='Item.Label')
#' g
#' plot_aggregation_grid(Lambda,triangle='right',bias_axes=FALSE,annotations=FALSE)
#' Lambda <- rank_topalpha_grid(Lambda,alpha=3,metrics)
#' plot_aggregation_grid(Lambda,by='TopAlpha')
#' g <- plot_aggregation_grid(Lambda,plotly_text = TRUE)
#' plotly::ggplotly(g, tooltip='text')
plot_aggregation_grid <- function(Lambda,by='Rank.Label',triangle="equilateral",bias_axes=TRUE,annotations=TRUE,plotly_text=FALSE,leg.pos='none',random.color=1) {
  # find column id of the chosen column and relabel for graphing purposes
  colid = which(colnames(Lambda)==by)
  colnames(Lambda)[colid]='label'
  # optional: randomize order of labels for better readability
  Lambda$label <- randomize_labels(Lambda$label,seed=random.color)
  # initial ggplot structure with theme
  g <- ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::theme(legend.position=leg.pos) 
  # Option 1: Equilateral transformation
  if(tolower(triangle)=="equilateral") {
    if(plotly_text) { # optional plotly text as part of aes()
      g <- g + 
        ggplot2::geom_point(data=Lambda, ggplot2::aes(x=equilambda1,y=equilambda2,color=label, #Rank.Label,
                                           text=paste("lambda:",round(equilambda1,2),round(equilambda2,2),"\n",by,":",label))) 
    } else {
      g <- g + 
        ggplot2::geom_point(data=Lambda, ggplot2::aes(x=equilambda1,y=equilambda2,color=label)) 
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
      g <- g + 
        ggplot2::geom_point(data=labels, ggplot2::aes(x=x, y=y),size=3) +
        ggplot2::geom_text(data=labels, ggplot2::aes(x=x, y=y+deltay, label=lab))+
        ggplot2::geom_label(ggplot2::aes(x=-0.5,y=0.5,label=paste("Total:",length(levels(Lambda$label))))) +
        ggplot2::xlim(c(-0.6,0.6)) + 
        ggplot2::ylim(c(-0.05,0.95))
    }
  }
  # Option 2: Right triangle
  if(tolower(triangle)=="right") {
    if(plotly_text) { # optional plotly text as part of aes()
      g <- g + 
        ggplot2::geom_point(data=Lambda, ggplot2::aes(x=lambda1,y=lambda2,color=label,
                                           text=paste("lambda:",round(equilambda1,2),round(equilambda2,2),"\n",by,":",label))) 
    } else {
      g <- g + 
        ggplot2::geom_point(data=Lambda, ggplot2::aes(x=lambda1,y=lambda2,color=label)) 
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
      g <- g + 
        ggplot2::geom_point(data=labels, ggplot2::aes(x=x, y=y),size=3) +
        ggplot2::geom_text(data=labels, ggplot2::aes(x=x, y=y+deltay, label=lab))+
        ggplot2::geom_label(ggplot2::aes(x=0.75,y=0.75,label=paste("Total:",length(levels(Lambda$label))))) +
        ggplot2::xlim(c(-0.1,1.1)) + 
        ggplot2::ylim(c(-0.05,1.05))
    }
  }
  return(g) 
}