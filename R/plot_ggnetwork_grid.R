#' Plot the network representation of weight set decomposition based on grid search via ggplot
#' 
#' Plot the dual graph of the weight set decomposition, where nodes indicate indifference regions and
#' edges represent adjacency. Computed based on the grid search. 
#' 
#' @param num_metrics The dimension of the problem indicating total number of metrics to be aggregated 
#' (3, 4, or 5)
#' @param Lambda Data frame containing weight vectors and labels for weighted rank aggregations.
#' Must include column names lambda1/2 for right triangle representation or 
#' equilambda1/2 for equilateral triangle representation. 
#' Must include column name Label for the string/factor identifier of the rank aggregation.
#' @param metrics Data frame for 3 metrics, one metric per column, one row per item, smaller values are better.
#' @param node_size Size of nodes. Default is 15
#' @param node_label TRUE/FALSE for whether to label nodes with the Rank.Label
#' @param node_color String to indicate how all nodes should be colored:
#' 'default', 'distance', 'volume', 'bias', 'equity', 'distance_from'
#' @param triangle Specify whether the nodes should be plotted with respect to the 
#' right triangle representation ("right") or the equilateral triangle representation ("equilateral"). 
#' @param leg.pos Input for ggplot legend positioning, e.g. 'bottom', 'right', or c(0.8,0.8)
#' @param random.color Seed for randomizing colors based on label (for improved readability).
#' 0 indicates no randomization.
#' @param distance_from Weight vector used for coloring when node_color = 'distance' 
#' @param show_bar Boolean for a progress bar printed in console
#' @return A list of 3 output used to construct the network: list of nodes, edges, and positions of nodes
#' @importFrom magrittr %>%
#' @export
#' 
#' @examples 
#' Lambda <- weight_set()
#' metrics <- data.frame('risk1'=c(1,2,3,4,5), 
#'                      'risk2'=c(2,3,1,5,4), 'risk3'=c(3,1,5,4,2))
#' Lambda <- rank_aggregation_grid(3,Lambda,metrics)
#' plot_aggregation_grid(Lambda,leg.pos='bottom')
#' plot_ggnetwork_grid(3, Lambda, metrics,leg.pos='bottom')
#' plot_ggnetwork_grid(3, Lambda, metrics, node_size=20, node_label=TRUE, edge_size=1.5)
plot_ggnetwork_grid <- function(num_metrics=3,Lambda,metrics,node_size=10,node_label=FALSE,node_color='default',
                                edge_size=1,triangle="equilateral",leg.pos='none',random.color=1,distance_from=NA,show_bar=FALSE) {
  
  # optional: randomize order of labels for better readability
  if(node_color=='default') Lambda$Rank.Label <- randomize_labels(Lambda$Rank.Label,seed=random.color)
  
  level_library=levels(Lambda$Rank.Label)
  n=length(level_library)
  if(num_metrics==3) {
    # identify the 3 extreme points
    extreme_labels = c(Lambda$Rank.Label[which(Lambda$lambda1==1)],
                       Lambda$Rank.Label[which(Lambda$lambda2==1)],
                       Lambda$Rank.Label[which(Lambda$lambda3==1)])
    # only for 3 metrics can we position the nodes to match the triangular weight set
    # plot the nodes based on the average weight of its indifference region
    if(tolower(triangle)=='right') {
      positions = Lambda %>%
        dplyr::group_by(Rank.Label) %>%
        dplyr::summarise(xpos = mean(lambda1), ypos = mean(lambda2))
    } else {
      positions = Lambda %>%
        dplyr::group_by(Rank.Label) %>%
        dplyr::summarise(xpos = mean(equilambda1), ypos = mean(equilambda2))
    }
  } else if(num_metrics==4) {
    # identify the 4 extreme points
    extreme_labels = as.character(c(
      Lambda$Rank.Label[which(Lambda$lambda1==1)],
      Lambda$Rank.Label[which(Lambda$lambda2==1)],
      Lambda$Rank.Label[which(Lambda$lambda3==1)],
      Lambda$Rank.Label[which(Lambda$lambda4==1)]))
    # plot the 4 extreme points at the corner of unit square, treat lambdas as convex multipliers
    # mu1*(0,0) + mu2*(1,0) + mu3*(0,1) + mu4*(1,1)
    positions = Lambda %>%
      dplyr::group_by(Rank.Label) %>%
      dplyr::summarise(mu1 = mean(lambda1), mu2 = mean(lambda2), mu3 = mean(lambda3), mu4 = mean(lambda4))
    jitter = runif(dim(positions)[1], max=0.05)
    positions$xpos = positions$mu2 + positions$mu4 + jitter
    positions$ypos = positions$mu3 + positions$mu4 + jitter
    # overwrite the positions of extreme points
    xrange = range(positions$xpos)
    yrange = range(positions$ypos)
    positions$xpos[which(positions$Rank.Label==extreme_labels[1])] <- xrange[1]
    positions$ypos[which(positions$Rank.Label==extreme_labels[1])] <- yrange[1]
    positions$xpos[which(positions$Rank.Label==extreme_labels[2])] <- xrange[2]
    positions$ypos[which(positions$Rank.Label==extreme_labels[2])] <- yrange[1]
    positions$xpos[which(positions$Rank.Label==extreme_labels[3])] <- xrange[1]
    positions$ypos[which(positions$Rank.Label==extreme_labels[3])] <- yrange[2]
    positions$xpos[which(positions$Rank.Label==extreme_labels[4])] <- xrange[2]
    positions$ypos[which(positions$Rank.Label==extreme_labels[4])] <- yrange[2]
  }
  # data frame for edges
  edges <- data.frame(i='none',j='none',ix=0,iy=0,jx=0,jy=0)
  print(paste("This may take some time. Adjacencies to check:",n*(n-1)/2,". Estimated time:",round(0.01*n*(n-1)/120,1),"minutes"))
  if(show_bar) {# optional progress bar using utils
    pb <- utils::txtProgressBar(width=20,style=3)
  } 
  counter = 0
  for(i in 1:(n-1)) {
    for(j in (i+1):n) {
      l1=level_library[i]
      l2=level_library[j]
      if(are_adjacent(num_metrics=3,l1,l2,metrics)) {
        edges <- rbind(edges,data.frame(i=l1,j=l2,
                                        ix=positions$xpos[i],iy=positions$ypos[i],
                                        jx=positions$xpos[j],jy=positions$ypos[j]))
      }
      counter = counter + 1
      if(counter%%50==0 && show_bar) utils::setTxtProgressBar(pb, counter/(n*(n-1)/2))
    }
  }
  edges=edges[-1,]
  # custom labels, colors, etc.
  ext_points <- subset(positions, Rank.Label%in%extreme_labels)
  if(num_metrics==3) ext_points$Label <- c('r1','r2','r3')
  if(num_metrics==4) ext_points$Label <- c('r1','r2','r3','r4')
  
  # optional: randomize order of labels for better readability
  if(node_color=='default') positions$Color <- positions$Rank.Label
  if(node_color=='distance') {
    # compute distance per node
    positions$Color = 0
    for(i in 1:dim(positions)[1]) {
      #d_squared = (positions$mu1[i] - distance_from[1])**2
      #d_squared = d_squared + (positions$mu2[i] - distance_from[2])**2
      #d_squared = d_squared + (positions$mu3[i] - distance_from[3])**2
      #if(num_metrics==4) d_squared = d_squared + (positions$mu4[i] - distance_from[4])**2
      if(num_metrics==3) positions$Color[i] = sqrt(sum((distance_from-positions[i,c('mu1','mu2','mu3')])**2))
      if(num_metrics==4) positions$Color[i] = sqrt(sum((distance_from-positions[i,c('mu1','mu2','mu3','mu4')])**2))
    }
    # find label for closest weight (first filter for faster search)
    subLambda <- subset(Lambda, lambda1 < distance_from[1]+0.1 & lambda1 > distance_from[1]-0.1 )
    subLambda <- subset(subLambda, lambda2 < distance_from[2]+0.1 & lambda2 > distance_from[2]-0.1 )
    subLambda <- subset(subLambda, lambda3 < distance_from[3]+0.1 & lambda3 > distance_from[3]-0.1 )
    subLambda$dist = 0
    for(i in 1:dim(subLambda)[1]) {
      if(num_metrics==3) subLambda$dist[i] = sqrt(sum((distance_from-subLambda[i,c('lambda1','lambda2','lambda3')])**2))
      if(num_metrics==4) subLambda$dist[i] = sqrt(sum((distance_from-subLambda[i,c('lambda1','lambda2','lambda3','lambda4')])**2))
    }
    closest_lab = subLambda$Rank.Label[which.min(subLambda$dist)]
  }
  if(node_color=='volume') {
    positions$Color = 0
    for(i in 1:dim(positions)[1]) {
      lab = positions$Rank.Label[i]
      positions$Color[i] = 100*sum(Lambda$Rank.Label==lab)/dim(Lambda)[1]
    }
  }
  
  # initial ggplot structure with theme
  g <- ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::theme(legend.position=leg.pos) 
  
  # triangular perimeter of weight set
  if(num_metrics==3 && tolower(triangle)=='right') {
    g <- g + ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = 0, yend = 1), color="black",linewidth=2) + 
      ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = 1, yend = 0), color="black",linewidth=2) + 
      ggplot2::geom_segment(ggplot2::aes(x = 0, y = 1, xend = 1, yend = 0), color="black",linewidth=2) 
  } else if(num_metrics==3 && tolower(triangle)=='equilateral') {
    g <- g + ggplot2::geom_segment(ggplot2::aes(x = -0.5, y = 0, xend = 0, yend = 0.5*sqrt(3)), color="black",linewidth=2) + 
      ggplot2::geom_segment(ggplot2::aes(x = -0.5, y = 0, xend = 0.5, yend = 0), color="black",linewidth=2) + 
      ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0.5*sqrt(3), xend = 0.5, yend = 0), color="black",linewidth=2) 
  }
  
  # plot edges first, then nodes, then labels
  g <- g + 
    ggplot2::geom_segment(data=edges,ggplot2::aes(x = ix, y = iy, xend = jx, yend = jy), color="grey", linewidth=edge_size) + 
    ggplot2::geom_point(data=positions, ggplot2::aes(x=xpos,y=ypos,fill=Color),size=node_size,shape=21,color='black') 
    
  
  if(node_color=='distance') {
    g <- g + ggplot2::geom_point(data=subset(positions, Rank.Label==as.character(closest_lab)), 
                                 ggplot2::aes(x=xpos,y=ypos,fill=Color),size=node_size,shape=21,color='red',stroke=2) +
            ggplot2::geom_point(data=subset(positions, Rank.Label==as.character(closest_lab)), 
                                ggplot2::aes(x=xpos,y=ypos),size=node_size,shape=8,color='red') +
            ggplot2::scale_fill_gradient2(name="Distance from point",low = "green",mid = "orange",high = "white",
                                    #midpoint=median(positions$Color))
                                    midpoint=mean(range(positions$Color)))
      
  }
  
  if(node_color=='volume') {
    g <- g + ggplot2::scale_fill_gradient2(name="Volume (%)",low = "white",mid = "orange",high = "green",
                                             #midpoint=median(positions$Color))
                                             midpoint=mean(range(positions$Color)))
  }
  # optional node labels
  if(node_label) {
    g <- g+ggplot2::geom_text(data=positions, ggplot2::aes(x=xpos, y=ypos, label=Rank.Label))
  } else {
    g <- g+ggplot2::geom_text(data=ext_points, ggplot2::aes(x=xpos, y=ypos, label=Label))
  }
  
  return(g)
}