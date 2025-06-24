#' Plot one position region for the grid search
#' 
#' Choose one of the items to show the item's ranked position for all weights in the weight set grid. 
#' 
#' @param Lambda Data frame containing weight vectors and labels for weighted rank aggregations.
#' Must include column names lambda1/2 for right triangle representation or 
#' equilambda1/2 for equilateral triangle representation. 
#' Must include column name Label for the string/factor identifier of the rank aggregation.
#' @param item The index for which item to represent.
#' @param position Either string 'all' to illustrate all positions or a specific numeric
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
#' @return A ggplot structure which can be plotted directly.  
#' @importFrom magrittr %>%
#' @export
#' 
#' @examples 
#' Lambda <- weight_set(0.01)
#' metrics <- data.frame('cost'=c(10,20,30,40), 'time'=c(5.9, 3.3, 2.5, 4.1), 'risk'=c(1,4,3,2))
#' Lambda <- rank_aggregation_grid(Lambda,metrics)
#' plot_position_grid(Lambda,item=1)
#' g <- plot_position_grid(Lambda,item=1,leg.pos='bottom')
#' g
#' plot_position_grid(Lambda,item=1,position=1)
#' plot_position_grid(Lambda,item=1,position='all',triangle='right',leg.pos='bottom')
plot_position_grid <- function(Lambda,item,position='all',triangle="equilateral",bias_axes=TRUE,annotations=TRUE,plotly_text=FALSE,leg.pos='none') {
  # initial ggplot structure with theme
  g <- ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::theme(legend.position=leg.pos) 
  # for every row in Lambda, parse and label the ranked position of item
  Lambda$Position = 0
  for(i in 1:dim(Lambda)[1]) {
    ranks = unlist(strsplit(as.character(Lambda$Rank.Label[i]), "[.]"))
    Lambda$Position[i] <- as.numeric(ranks[item])
  }
  #Lambda$Position <- as.factor(Lambda$Position)
  freq=Lambda %>% dplyr::count(Position, sort=TRUE)
  if(position=='all') {
    most=freq$Position[1]
    pct=round(100*sum(Lambda$Position==most)/dim(Lambda)[1],2)
    plotdf <- Lambda
  } else {
    pct=round(100*sum(Lambda$Position==position)/dim(Lambda)[1],2)
    plotdf <- subset(Lambda,Position==position)
  }
  
  
  # Option 1: Equilateral transformation
  if(tolower(triangle)=="equilateral") {
    if(plotly_text) { # optional plotly text as part of aes()
      g <- g + 
        ggplot2::geom_point(data=plotdf, ggplot2::aes(x=equilambda1,y=equilambda2,color=Position,
                                                     text=paste("lambda:",round(equilambda1,2),round(equilambda2,2),"\nposition:",Position))) 
    } else {
      g <- g + 
        ggplot2::geom_point(data=plotdf, ggplot2::aes(x=equilambda1,y=equilambda2,color=Position)) 
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
        ggplot2::xlim(c(-0.6,0.6)) + 
        ggplot2::ylim(c(-0.05,0.95))
      
      if(position=='all') {
        g <- g+ 
          ggplot2::geom_label(ggplot2::aes(x=-0.5,y=0.5,label=paste("Most Common: ",most,"\n(", pct,"%)")))
      } else {
        g <- g+ 
          ggplot2::geom_label(ggplot2::aes(x=-0.5,y=0.5,label=paste("Item: ",item,"\nPosition: ",position,"\n(", pct,"%)")))
      }
    }
  }
  # Option 2: Right triangle
  if(tolower(triangle)=="right") {
    if(plotly_text) { # optional plotly text as part of aes()
      g <- g + 
        ggplot2::geom_point(data=plotdf, ggplot2::aes(x=lambda1,y=lambda2,color=Position,
                                                     text=paste("lambda:",round(equilambda1,2),round(equilambda2,2),"\nposition:",Position))) 
    } else {
      g <- g + 
        ggplot2::geom_point(data=plotdf, ggplot2::aes(x=lambda1,y=lambda2,color=Position)) 
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
        ggplot2::xlim(c(-0.1,1.1)) + 
        ggplot2::ylim(c(-0.05,1.05))
      
      if(position=='all') {
        g <- g+ 
          ggplot2::geom_label(ggplot2::aes(x=0.75,y=0.75,label=paste("Most Common: ",most,"\n(", pct,"%)")))
      } else {
        g <- g+ 
          ggplot2::geom_label(ggplot2::aes(x=0.75,y=0.75,label=paste("Item: ",item,"\nPosition: ",position,"\n(", pct,"%)")))
      }
    }
  }
  # color gradient and legend
  rg <- range(Lambda$Position)
  g <- g+ ggplot2::scale_color_gradientn(colours = grDevices::terrain.colors(10),
                          #gradient2()#low ="blue",mid = "white",high = "red",
                         #n.breaks = 1+max(Lambda$Position)-min(Lambda$Position)
                         breaks = c(rg[1],round(mean(rg)),rg[2])
                         )

  return(g)
}
  