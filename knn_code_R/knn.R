#
#
library(foreach)

knn_impute_single <- function(rmat, D, ind, k, common=0){
  
  # get row and col indices from ind
  i = ind[1]
  j = ind[2]
  
  # get distances between row i and all rows m != i
  n = nrow(rmat)
  d_low <- foreach(m=1:(i-1), .combine='c') %dopar% D[n*(m-1) - m*(m-1)/2 + i-m] # m < i
  d_high <- foreach(m=(i+1), .combine='c') %dopar% D[n*(i-1) - i*(i-1)/2 + m-i] # i < m
  d = c(d_low,d_high)
  
  # if we are not checking for number of common neighbors
  if(common ==0){
    neighbors = sort(d, na.last=NA, index.return=TRUE)[1:k]
    distances = d[neighbors]
    
    # check that there are enough neighbors
    if(is.na(max(distances))){
      distances = distances[!is.na(distances)]
      neighbors = neighbors[!is.na(distances)]
      cat('Warning: less than k neighbors for (',i,',',j,')\n')
    }
    
  } else {
    # if we are checking for common neighbors,
    # find k nearest neighbors that have at least c common ratings
    neighbors = rep(0,k)
    distances = rep(0,k)
    count = 1
    
    # until we have k neighbors
    while(count <= k){
      
      # get rows with lowest distance
      candidates = which(d==min(d, na.rm=TRUE),arr.ind=TRUE)
      
      # stop if lowest distance is infinity
      if(d[candidates[1]] == Inf){
        neighbors = neighbors[1:count]
        distances = distances[1:count]
        cat('Warning: less than k neighbors for (',i,',',j,')\n')
        break
      }
      
      # for rows with lowest distance (candidates)
      for(cand in candidates){
        
        # compute number of common ratings
        com = length(intersect(which(rmat[i,]!=0, arr.ind=TRUE), which(rmat[cand,]!=0, arr.ind=TRUE)))
        
        # add candidate as nearest neighbor if sufficiently many common ratings
        if(com >= common){
          neighbors[count+1] = cand
          distances[count+1] = d[cand]
          count = count + 1
          if(count > k) break
        }
      } # end iteration through candidates
      d[candidates] = Inf # mark candidates so that they are not picked again
      
    } # end while fillig up k nearest neighbors
  } # end else
  
  # impute value using weighted mean
  weights = 1 / (distances * sum(1/distances))
  impute = t(weights) %*% rmat[neighbors,j]
  return (impute)
}