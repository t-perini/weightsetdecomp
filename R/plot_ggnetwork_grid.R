#' Plot the network representation of weight set decomposition based on grid search via ggplot
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
#' @param random.color Seed for randomizing colors based on label (for improved readability).
#' 0 indicates no randomization.
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
#' plot_ggnetwork_grid(Lambda, metrics,leg.pos='bottom')
#' plot_ggnetwork_grid(Lambda, metrics, node_size=20, node_label=TRUE, edge_size=1.5)
plot_ggnetwork_grid <- function(Lambda,metrics,node_size=10,node_label=FALSE,edge_size=1,triangle="equilateral",leg.pos='none',random.color=1) {
  # optional: randomize order of labels for better readability
  Lambda$Rank.Label <- randomize_labels(Lambda$Rank.Label,seed=random.color)
  
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
  #positions_matrix = as.matrix(positions[,c('xpos','ypos')])
  # data frame for edges
  edges <- data.frame(i='none',j='none',ix=0,iy=0,jx=0,jy=0)
  for(l1 in 1:(n-1)) {
    for(l2 in l1:n) {
      i=level_library[l1]
      j=level_library[l2]
      if(are_adjacent(i,j,metrics)) {
        edges <- rbind(edges,data.frame(i=i,j=j,
                                        ix=positions$xpos[l1],iy=positions$ypos[l1],
                                        jx=positions$xpos[l2],jy=positions$ypos[l2]))
      }
    }
  }
  edges=edges[-1,]
  # initial ggplot structure with theme
  g <- ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::theme(legend.position=leg.pos) 
  
  # perimeter of weight set
  if(tolower(triangle)=='right') {
    g <- g + ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = 0, yend = 1), color="black",linewidth=2) + 
      ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = 1, yend = 0), color="black",linewidth=2) + 
      ggplot2::geom_segment(ggplot2::aes(x = 0, y = 1, xend = 1, yend = 0), color="black",linewidth=2) 
  } else {
    g <- g + ggplot2::geom_segment(ggplot2::aes(x = -0.5, y = 0, xend = 0, yend = 0.5*sqrt(3)), color="black",linewidth=2) + 
      ggplot2::geom_segment(ggplot2::aes(x = -0.5, y = 0, xend = 0.5, yend = 0), color="black",linewidth=2) + 
      ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0.5*sqrt(3), xend = 0.5, yend = 0), color="black",linewidth=2) 
  }
  
  # nodes and edges
  g <- g + 
    ggplot2::geom_segment(data=edges,ggplot2::aes(x = ix, y = iy, xend = jx, yend = jy), color="gray", size=edge_size) + 
    ggplot2::geom_point(data=positions, ggplot2::aes(x=xpos,y=ypos,color=Rank.Label),size=node_size) 
  # optional node labels
  if(node_label) {
    g <- g+ggplot2::geom_text(data=positions, ggplot2::aes(x=xpos, y=ypos, label=Rank.Label))
  }
  
  return(g)
}