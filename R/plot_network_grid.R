#' Plot the network representation of weight set decomposition based on grid search
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
#' @param triangle Specify whether the nodes should be plotted with respect to the 
#' right triangle representation ("right") or the equilateral triangle representation ("equilateral"). 
#' @param leg.pos Input for ggplot legend positioning, e.g. 'bottom', 'right', or c(0.8,0.8)
#' @param show_bar Boolean for a progress bar printed in console
#' @return A list of 3 output used to construct the network: list of nodes, edges, and positions of nodes
#' @importFrom magrittr %>%
#' @export
#' 
#' @examples 
#' Lambda <- weight_set(0.01)
#' metrics <- data.frame('risk1'=c(1,2,3,4,5), 
#'                      'risk2'=c(2,3,1,5,4), 'risk3'=c(3,1,5,4,2))
#' Lambda <- rank_aggregation_grid(Lambda,metrics)
#' plot_aggregation_grid(Lambda,leg.pos='bottom')
#' plot_network_grid(Lambda, metrics, node_label=TRUE)
#' plot_network_grid(Lambda, metrics, node_size=5, node_label=FALSE, triangle='right')
plot_network_grid <- function(num_metrics=3,Lambda,metrics,node_size=15,node_label=FALSE,edge_size=1,triangle="equilateral",leg.pos='none',show_bar=FALSE) {
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
    positions_matrix = as.matrix(positions[,c('xpos','ypos')])
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
    positions_matrix = as.matrix(positions[,c('xpos','ypos')])
  }
  # construct adjacency matrix
  adj <- data.frame(i="none",j="none")
  print(paste("This may take some time. Adjacencies to check:",n*(n-1)/2,". Estimated time:",round(0.01*n*(n-1)/120,1),"minutes"))
  if(show_bar) {# optional progress bar using utils
    pb <- utils::txtProgressBar(width=20,style=3)
  } 
  counter = 0
  for(i in 1:(n-1)) {
    for(j in i:n) {
      l1=level_library[i]
      l2=level_library[j]
      if(are_adjacent(num_metrics,l1,l2,metrics)) {
        adj <- rbind(adj,data.frame(i=l1,j=l2))
      }
      counter = counter + 1
      if(counter%%50==0 && show_bar) utils::setTxtProgressBar(pb, counter/(n*(n-1)/2))
    }
  }
  adj=adj[-1,]
  network <- igraph::graph_from_data_frame(adj,directed=FALSE,vertices=level_library)
  # report number of connected components
  print(paste('Connected components:',igraph::count_components(network)))
  # custom labels for extreme points only
  custom_vlabels <- c()
  for(lab in igraph::V(network)$name) {
    if(lab%in%extreme_labels) {
      r_i <- paste0('r',which(lab==extreme_labels))
      custom_vlabels <- c(custom_vlabels,r_i)
    } else {
      custom_vlabels <- c(custom_vlabels,NA)
    }
  }
  if(num_metrics>3) {
    plot(network,layout=positions_matrix,vertex.size=node_size,frame.color=NA,vertex.label=custom_vlabels)
  } else if(node_label==TRUE) {
    plot(network,layout=positions_matrix,vertex.size=node_size,frame.color=NA)
  } else {
    plot(network,layout=positions_matrix,vertex.size=node_size,frame.color=NA,vertex.label=custom_vlabels)
  }
  return(list(level_library,adj,positions,network))
}