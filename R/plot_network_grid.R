#' Plot the network representation of weight set decomposition based on grid search
#' 
#' Plot the dual graph of the weight set decomposition, where nodes indicate indifference regions and
#' edges represent adjacency. Computed based on the grid search.
#' 
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
plot_network_grid <- function(Lambda,metrics,node_size=15,node_label=FALSE,edge_size=1,triangle="equilateral",leg.pos='none') {
  level_library=levels(Lambda$Rank.Label)
  n=length(level_library)
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
  # construct adjacency matrix
  adj <- data.frame(i="none",j="none")
  for(i in 1:(n-1)) {
    for(j in i:n) {
      l1=level_library[i]
      l2=level_library[j]
      if(are_adjacent(l1,l2,metrics)) {
        adj <- rbind(adj,data.frame(i=l1,j=l2))
      }
    }
  }
  adj=adj[-1,]
  network <- igraph::graph_from_data_frame(adj,directed=FALSE,vertices=level_library)
  if(node_label==TRUE) {
    plot(network,layout=positions_matrix,vertex.size=node_size,frame.color=NA)
  } else {
    plot(network,layout=positions_matrix,vertex.size=node_size,frame.color=NA,vertex.label=NA)
  }
  return(list(level_library,adj,positions))
}