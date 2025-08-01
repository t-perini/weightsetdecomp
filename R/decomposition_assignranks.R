#' Assigned rank labels to points for exact rank aggregation decomposition
#' 
#' Every point is labeled with a valid rank label from grid search (either full rank.label or topalpha). 
#' Step 3 of [rank_decomposition_exact]. 
#' Correctness depends on the completeness of the Rank.Label list from [rank_aggregation_grid] 
#' or TopAlpha list form [rank_topalpha_grid], 
#' which may require recomputing with finer step size. 
#'  
#' @param input_data Data frame containing 3 columns of ranks/ratings per item. 
#' Each row represents one item. 
#' @param pointdf Data frame, ideally from [decomposition_intersections], 
#' with two columns (x and y) where each row gives a point in the weight set.
#' @param Lambda Data frame from [rank_aggregation_grid] including weights and ranked labels
#' @param alpha The value for alpha if computing top-alpha regions
#' @param show_bar Boolean for a progress bar and time data printed in console
#' @return A data frame of three columns (lambda1, lambda2, and Rank.Label) for all points belonging to each
#' indifference region. 
#' @export
#' @examples 
#' Lambda <- weight_set(stepsize=0.1)
#' rankdf <- data.frame('rank1'=c(1,2,3,4,5), 
#'            'rank2'=c(2,3,1,5,4), 'rank3'=c(3,1,5,4,2))
#' Lambda <- rank_aggregation_grid(3,Lambda,rankdf)
#' Linedf = decomposition_linesegs(rankdf)
#' IntersectPts = decomposition_intersections(Linedf)
#' IRextremepts = decomposition_assignranks(rankdf,IntersectPts,Lambda)
#' Lambda <- rank_topalpha_grid(Lambda,alpha=3,rankdf)
#' IRextremepts = decomposition_assignranks(rankdf,IntersectPts,Lambda,alpha=3)
decomposition_assignranks <- function(input_data,pointdf,Lambda,alpha=NaN,show_bar=FALSE) {
  nrows=dim(pointdf)[1]  
  if(is.na(alpha)) {
    lablist <- levels(Lambda$Rank.Label)
  } else {
    lablist <- levels(Lambda$TopAlpha)
  }
  
  l=length(lablist)
  
  if(show_bar) {
    estTime=max(round(0.0012*nrows*l,1),0.1)
    print(paste("Step 3: Assign rankings to ",nrows,"  points \n Operations to perform:",nrows*l,". Estimated time:",estTime,"seconds."))
    pb <- utils::txtProgressBar(width=20,style=3)
    tictoc::tic()
  }
  
  rank1=input_data[,1]
  rank2=input_data[,2]
  rank3=input_data[,3]
  
  # weights are labeled with rank and will be extreme points of the indifference region
  IRextremepts <- data.frame(lambda1=0, lambda2=0, Rank.Label = "blank")
  
  count=0
  for(lab in lablist) {
    num=0
    if(is.na(alpha)) {
      subdf <- subset(Lambda,Rank.Label==lab)
    } else {
      subdf <- subset(Lambda,TopAlpha==lab)
    }
    
    # use a median (interior) point of the indifference region to compute ranking without ties
    meanx = mean(subdf$lambda1)
    meany = mean(subdf$lambda2)
    meanrating = meanx*rank1+meany*rank2+(1-meanx-meany)*rank3
    #meanrank is the true ranking for the IR
    #meanrank = rank(meanrating, ties.method="first")
    meanrank = flex_rank(meanrating,ties=TRUE)
    for(i in 1:dim(pointdf)[1]) {
      lambda=c(pointdf[i,1], pointdf[i,2], 1-pointdf[i,1]-pointdf[i,2])
      lambdarating = lambda[1]*rank1+lambda[2]*rank2+lambda[3]*rank3
      if(is.na(alpha)) {
        #test if meanrank is true for lambdarating (note that a robust test accounts for ties)
        test=TRUE
        j=1
        while(test==TRUE && j<length(meanrank)) {
          if(lambdarating[which(meanrank==j)] - lambdarating[which(meanrank==(j+1))] > 0.001) test=FALSE
          j=j+1
        }
        if(test==TRUE) {
          IRextremepts=rbind(IRextremepts,c(pointdf[i,1],pointdf[i,2], lab))
          num=num+1
        }
      } else {
        #test if lamdaranking has rank <= alpha wherever meanrank has rank <= alpha
        testinds = which(meanrank<=alpha)
        lambdaranking = flex_rank(lambdarating,ties=TRUE)
        if(all(lambdaranking[testinds]<=alpha)) {
          IRextremepts=rbind(IRextremepts,c(pointdf[i,1],pointdf[i,2], lab))
          num=num+1
        } 
      }
    }
    count=count+1
    if(show_bar && count%%10==0) utils::setTxtProgressBar(pb, count/length(lablist))
  }
  
  if(show_bar) {
    utils::setTxtProgressBar(pb, 1)
    close(pb)
    time=tictoc::toc()
    print(paste("Total time spent:",time[[4]]))
  }
  
  # post-processing
  IRextremepts = IRextremepts[-1,]
  IRextremepts$lambda1 <- as.numeric(IRextremepts$lambda1)
  IRextremepts$lambda2 <- as.numeric(IRextremepts$lambda2)
  IRextremepts$Rank.Label <- as.factor(IRextremepts$Rank.Label)
  return(IRextremepts)
}