#' Plot a histogram of indifference region areas from exact decomposition
#' 
#' Using the labeled rank aggregations from the exact decomposition, [rank_decomposition_exact], 
#' plot the area of each 
#' indifference region as a percentage of the entire weight set. 
#' 
#' @param IR_hull Data frame from [rank_decomposition_exact] containing weight vectors and labels for each indifference region. 
#' Must include column name Rank.Label for the string/factor identifier of the rank aggregation. 
#' @param orientation 'vertical' or 'horizontal' for the type of bar graph.
#' @param min_threshold Number between 0 and 100 which specifies a minimum threshold of area (percentage). 
#' Any regions with area less than the threshold is omitted from the histogram. 
#' @param axis_labels Specify whether axis should be labeled by the complete ranked list
#' @param leg.pos Input for ggplot legend positioning, e.g. 'bottom', 'right', or c(0.8,0.8)
#' @return list containing: [[1]] A ggplot structure which can be plotted directly and 
#' [[2]] the data frame with estimated areas   
#' @export
#' 
#' @examples
#' Lambda <- weight_set(stepsize=0.1)
#' rankdf <- data.frame('rank1'=c(1,2,3,4,5), 
#'            'rank2'=c(2,3,1,5,4), 'rank3'=c(3,1,5,4,2))
#' Lambda <- Lambda <- rank_aggregation_grid(3,Lambda,rankdf)
#' inputlist <- rank_decomposition_exact(rankdf,Lambda)
#' IR_hull <- inputlist[[4]]
#' outlist <- plot_histogram_exact(IR_hull)
#' outlist[[1]] # for ggplot
#' outlist[[2]] # for data frame
#' plot_histogram_exact(IR_hull,orientation='vertical',min_threshold=1)[[1]]
#' plot_histogram_exact(IR_hull,min_threshold=1,axis_labels=FALSE,leg.pos='bottom')[[1]]
plot_histogram_exact <- function(IR_hull,orientation='horizontal',min_threshold=0,axis_labels=TRUE,leg.pos='none') {
  Areadf <- data.frame(Rank.Label=levels(IR_hull$Rank.Label), area=0)
  for(i in 1:dim(Areadf)[1]) {
    subdf <- subset(IR_hull, Rank.Label==Areadf$Rank.Label[i])
    subdf <- rbind(subdf[,c("lambda1","lambda2")], subdf[1,c("lambda1","lambda2")])
    box.hpts <- as.matrix(subdf)
    chull.poly <- sp::Polygon(box.hpts, hole=F)
    Areadf$area[i]=chull.poly@area
  }
  Areadf$Percent = round(100*Areadf$area/sum(Areadf$area),1) 
  max_pct = max(Areadf$Percent)
  subdf <- subset(Areadf,Percent>=min_threshold)
  
  
  # initial ggplot structure 
  g <- ggplot2::ggplot() + 
    ggplot2::xlab("") + ggplot2::ylab("") +
    ggplot2::theme(legend.position=leg.pos) 
  
  # Option 1: Horizontal bar graph
  if(orientation=='horizontal') {
    g <- g + 
      ggplot2::geom_col(data=subdf, ggplot2::aes(x=Percent,y=reorder(Rank.Label,Percent),fill=Rank.Label)) +
      ggplot2::geom_text(data=subdf, ggplot2::aes(x=Percent+1,y=reorder(Rank.Label,Percent),label=paste0(Percent,"%")),size=3,hjust=0) +
      ggplot2::xlim(c(0, 1.1*max_pct)) 
    
    if(axis_labels) {
      g<- g+ ggplot2::theme(axis.text.y = ggplot2::element_text(vjust = 0.5, hjust=0)) 
    } else {  
      g<- g+ ggplot2::theme(axis.text.y = ggplot2::element_blank())
    }
  }
  
  # Option 2: Vertical bar graph
  if(orientation=='vertical') {
    g <- g + 
      ggplot2::geom_bar(data=subdf, ggplot2::aes(x=reorder(Rank.Label,-Percent),y=Percent,fill=Rank.Label),stat="identity") +
      ggplot2::geom_text(data=subdf, ggplot2::aes(x=reorder(Rank.Label,-Percent),y=Percent+1,label=paste0(Percent,"%")),size=3) +
      ggplot2::ylim(c(0, 1.1*max_pct)) 
    
    if(axis_labels) {
      g<- g+ ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -45, vjust = 0, hjust=0.1)) 
    } else {
      g<- g+ ggplot2::theme(axis.text.x = ggplot2::element_blank())
    }
  }
  g
  return(list(g,Areadf))
}