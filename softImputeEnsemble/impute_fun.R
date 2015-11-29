library('Matrix')
library('softImpute')

max_lambda <- function(M, row_size, col_size, test_num, max_iter=100){
  # max_lambda()
  # A function to get the maximum lambda for cv in ensemble low
  # rank matrix complete
  #
  # Parameters
  # -------------
  # M         --> incomplete data matrix of type Incomplete
  # row_size  --> number of rows in ensemble sub-matrix
  # col_size  --> number of columns in ensemble sub-matrix
  # test_num  --> number of ensemble sub-matrices to test for lambda_0
  # max_iter  --> maximum iterations for lambda_0 function
  #
  # Returns
  # -------------
  # lambda    --> maximum labmda to try
  #
  
  center = TRUE
  
  n=dim(M)[1]
  p=dim(M)[2]
  
  # initialize lambdas
  lambdas <- rep(0, test_num)
  
  # for each iteration
  for(i in 1:test_num){
    
    # generate random submatrix, A
    rand_row <- sample.int(n, size=row_size, replace=FALSE)
    rand_col <- sample.int(p, size=col_size, replace=FALSE)
    A <- M[rand_row, rand_col]
    
    # mean center
    if(center){
      mean <- mean(A[which(A!=0, arr.ind=TRUE)])
      A <- A - mean
    }
    A <- as.matrix(A)
    A <- as(A, 'Incomplete')
    
    lambdas[i] <- lambda0(A, maxit=max_iter)
  }
  
  # return average lambda max as lambda_0
  lambda_max = mean(lambdas)
  return(lambda_max)
}
gen_kfold <- function(ratings, k){
  # gen_kfold()
  # A function to generate k-folds to be used
  # in k-fold cross-validation
  #
  # Parameters
  # -------------
  # ratings --> the data to be partitioned
  # k       --> number of folds to use
  #
  # Returns
  # -------------
  # S       --> a list of indices for each k-fold
  #
  
  # observable data to partition into k-folds
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

ensemble_cross_val <- function(ratings, kfold, lambda_seq, row_size, col_size, iter, rank.max, max_iter=300){
  # ensemble_cross_val()
  # A function to cross validate for the optimal
  # lambda in ensemble low rank matrix completion 
  #
  # Parameters
  # -------------
  # ratings     --> rating data
  # kfold       --> number of folds to be used in k-fold cross validation
  # lambda_seq  --> the sequence of lambda values to be cross validated
  # row_size    --> number of rows in ensemble sub-matrix
  # col_size    --> number of columns in ensemble sub-matrix
  # iter      --> number of ensemble sub-matrices to complete
  # rank.max  --> restricts rank of ensemble sub-matrix solution
  # max_iter  --> maximum iterations for individual completion
  #
  # Returns
  # -------------
  # lambda    --> optimal lambda
  #
  
  center = TRUE
  
  # get the column mean predictions
  rmat = sparseMatrix(i=ratings[,1],j=ratings[,2],x=ratings[,3])
  Pnum = apply(rmat!=0,2,sum)
  Psum = apply(rmat,2,sum)
  Pmeans = Psum / Pnum
  rm(rmat, Pnum, Psum) # remove variables -- save memory
  
  # generate kfolds
  S <- gen_kfold(ratings, kfold)
  
  # initialize mse
  mse = matrix(0,kfold,length(lambda_seq))
  
  for(i in 1:kfold){
    
    # hold out for this kfold
    fold_rows <-ratings[-S[[i]],1]
    fold_cols <- ratings[-S[[i]],2]
    fold_val <- ratings[-S[[i]],3]
    A <- Incomplete(fold_rows, fold_cols, fold_val)
    
    # center?
    if(center){
      A <- A - mean(fold_val)
    }
    
    warm_start = NULL
    # for each lambda
    for(j in 1:length(lambda_seq)){
      
      # predict values and fill in missing predictions with column means
      # I removed warm.start arguement: i <= (k - num_obs%%k ), since it appears to be unused
      pred_vals <- ensemble_impute(A, pred_ind=ratings[S[[i]],1:2], row_size=row_size, col_size=col_size, iter=iter, lambda=lambda_seq[j], rank.max=rank.max, max_iter=max_iter)
      na_ind <- which(is.na(pred_vals),arr.ind=TRUE) # yikes, sorry berk
      pred_vals[na_ind] <- Pmeans[ ratings[ S[[i]][na_ind],2] ] # fill in with column means (is this right??) 
      
      # record MSE
      mse[i,j] <- mean((fold_val-pred_vals)^2)
    }
  }
  
  # take mean of MSE and return lowest
  mean_mse <- apply(mse, 2, mean)
  best_lam <- lambda_seq[which.min(mean_mse)]
  return(list(best_lam=best_lam, mse=mse))
}

ensemble_impute <- function(M, pred_ind, row_size, col_size, iter, lambda, rank.max, max_iter=300) {
  
  # ensemble_impute()
  # A function to perform ensemble low-rank matrix completion 
  #
  # Parameters
  # -------------
  # M         --> incomplete data matrix of type Incomplete
  # preds_ind --> entries to predict
  # row_size  --> number of rows in ensemble sub-matrix
  # col_size  --> number of columns in ensemble sub-matrix
  # iter      --> number of ensemble sub-matrices to complete
  # lambda    --> low rank penalty parameter
  # rank.max  --> restricts rank of ensemble sub-matrix solution
  # preds     --> the indices of predictions
  # max_iter  --> maximum iterations
  #
  # Returns
  # -------------
  # pred_vals --> predicted values (could contain NaN)
  #
  
  center = TRUE
  
  # create matrices to store imputed values and counts
  n <- nrow(M)
  p <- ncol(M)
  val_mat <- matrix(0,n,p)
  count_mat <- matrix(0,n,p) 
  svd_obj <- NULL
  
  # for each iteration
  for(i in 1:iter){
    
    # generate random submatrix, A
    rand_row <- sample.int(n, size=row_size, replace=FALSE)
    rand_col <- sample.int(p, size=col_size, replace=FALSE)
    A <- M[rand_row, rand_col]
    
    # mean center
    if(center){
      mean <- mean(A[which(A!=0, arr.ind=TRUE)])
      A <- A - mean
    }
    
    # impute!
    A<-as.matrix(A)
    A <- as(A, "Incomplete")
    svd_obj <- softImpute(Y, rank.max=rank.max, lambda=lambda, type='als', maxit=max_iter)
    A_hat <- complete(A, svd_obj, unscale=TRUE)
    
    # update value and count matrices
    val_mat[rand_row, rand_col] <- val_mat[rand_row, rand_col] + A_hat
    count_mat[rand_row, rand_col] <- count_mat[rand_row, rand_col] + 1
    
  }
  
  # return predicted entries 
  pred_mat <- val_mat / count_mat
  pred_vals <- pred_mat[pred_ind[,1], pred_ind[,2]]
  return(pred_vals) 
}
