# Chris Harshaw and Berk "The Monster" Norman
# STAT 444
# Project

gen_kfold <- function(ratings, k){

  ## observable data to partition into k-folds
  num_obs = dim(ratings)[1]
  d.avail <- sample(1:num_obs)

  size1 <- floor(num_obs / k) # different sizes of partitioned sets
  size2 <- floor(num_obs / k) + 1

  S <- list() # initialize list

  # for each parition
  for(i in 1:k ){
    if(i <= (k - num_obs%%k ) ){

      ind <- d.avail[1:size1] # assign size 1 if k <= k - D mod k
      S[[i]] <- cbind(ratings[ind,1], ratings[ind,2], ind)
    }
    else{

      ind <- d.avail[1:size2] # assign size 2 if k > k - D mod k
      S[[i]]<- cbind(ratings[ind,1], ratings[ind,2], ind)
    }
    d.avail <- setdiff(d.avail,ind) # remove assigned portion
  }
  return(S)
}
