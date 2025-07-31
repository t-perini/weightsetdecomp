#' Plot a histogram of top-alpha region areas from grid search
#' 
#' Using the labeled rank aggregations from the grid search, [rank_topalpha_grid], 
#' plot the estimated area of each 
#' top-alpha region as a percentage of the entire weight set. 
#' Note that grid search is not guaranteed to capture every possible weighted 
#' rank aggregation nor to accurately estimate the total indifference region area. 
#' The stepsize of grid search determines the accuracy of these estimates.  
#' Smaller step size of the grid leads to capturing more indifference regions (with better 
#' estimation of area) at the cost of slower computations. 
#' 
#' @param Lambda Data frame containing weight vectors and labels for weighted rank aggregations.
#' Must include column names lambda1/2.  
#' Must include column name Label for the string/factor identifier of the rank aggregation.
#' @param orientation 'vertical' or 'horizontal' for the type of bar graph.
#' @param min_threshold Number between 0 and 100 which specifies a minimum threshold of area (percentage). 
#' Any regions with area less than the threshold is omitted from the histogram. 
#' @param axis_labels Specify whether axis should be labeled by the complete ranked list
#' @param leg.pos Input for ggplot legend positioning, e.g. 'bottom', 'right', or c(0.8,0.8)
#' @return list containing: [[1]] A ggplot structure which can be plotted directly and 
#' [[2]] the data frame with estimated areas   
#' @importFrom magrittr %>%
#' @export
#' 
#' @examples
#' Lambda <- weight_set()
#' metrics <- data.frame('cost'=c(10,20,30,40,50), 
#'            'time'=c(5.9, 3.3, 2.5, 4.1, 1.8), 'risk'=c(1,4,3,2,5))
#' Lambda <- rank_topalpha_grid(Lambda,alpha=3,metrics)
#' outlist <- plot_histogram_topalpha(Lambda)
#' outlist[[1]] # for ggplot
#' outlist[[2]] # for data frame
#' plot_histogram_topalpha(Lambda,orientation='vertical',min_threshold=1)[[1]]
#' plot_histogram_topalpha(Lambda,min_threshold=1,axis_labels=FALSE,leg.pos='bottom')[[1]]
plot_histogram_topalpha <- function(Lambda,orientation='horizontal',min_threshold=0,axis_labels=TRUE,leg.pos='none') {
  Areadf <- Lambda %>% dplyr::count(TopAlpha) 
  totalN = sum(Areadf$n)
  Areadf$Percent = round(100*Areadf$n/totalN,1)
  subdf <- subset(Areadf,Percent>=min_threshold)
  
  
  # initial ggplot structure 
  g <- ggplot2::ggplot() + 
    ggplot2::xlab("") + ggplot2::ylab("") +
    ggplot2::theme(legend.position=leg.pos) 
  
  # Option 1: Horizontal bar graph
  if(orientation=='horizontal') {
    g <- g + 
      ggplot2::geom_col(data=subdf, ggplot2::aes(x=Percent,y=reorder(TopAlpha,n),fill=TopAlpha)) +
      ggplot2::geom_text(data=subdf, ggplot2::aes(x=Percent+1,y=reorder(TopAlpha,n),label=paste0(Percent,"%")),size=3,hjust=0) +
      ggplot2::xlim(c(0, 1.1*max(subdf$Percent))) 
    
    if(axis_labels) {
      g<- g+ ggplot2::theme(axis.text.y = ggplot2::element_text(vjust = 0.5, hjust=0)) 
    } else {  
      g<- g+ ggplot2::theme(axis.text.y = ggplot2::element_blank())
    }
  }
  
  # Option 2: Vertical bar graph
  if(orientation=='vertical') {
    g <- g + 
      ggplot2::geom_bar(data=subdf, ggplot2::aes(x=reorder(TopAlpha,-n),y=Percent,fill=TopAlpha),stat="identity") +
      ggplot2::geom_text(data=subdf, ggplot2::aes(x=reorder(TopAlpha,-n),y=Percent+1,label=paste0(Percent,"%")),size=3) +
      ggplot2::ylim(c(0, 1.1*max(subdf$Percent))) 
    
    if(axis_labels) {
      g<- g+ ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -45, vjust = 0, hjust=0.1)) 
    } else {
      g<- g+ ggplot2::theme(axis.text.x = ggplot2::element_blank())
    }
  }
  
  return(list(g,Areadf))
}