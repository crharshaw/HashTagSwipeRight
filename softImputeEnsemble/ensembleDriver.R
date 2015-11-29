require("Matrix")
library("softImpute")
source("impute_fun.R")

# import data
ratings = read.table("../data/ratings.csv",header=TRUE,sep=",")
idmap = read.table("../data/IDMap.csv",header=TRUE,sep=",")

# set parameters
lambda_min = 1e-3
lambda_num= 20
rank.max = 80
row_size = 200
col_size = 200
kfold = 5
iter = 5e4

# find upper lambda
M = Incomplete(i=ratings[,1], j=ratings[,2], x=ratings[,3])
test_num = 25
lambda_max = max_lambda(M, row_size, col_size, test_num)
lambda_seq = rev(10^ seq(log10(lambda_min), log10(lambda_max), length.out=lambda_num))

# cross validate to get the best lambda
cv_info <- ensemble_cross_val(ratings, kfold, lambda_seq, row_size, col_size, iter, rank.max)
lambda <- cv_info$best_lam

# maybe plot cv curves here? using cv_info$mse

# predict the ratings with ensemble method!
pred_ind = idmap[,1:2]
pred_vals <- ensemble_impute(M, pred_ind=pred_ind, row_size, col_size, iter, lambda, rank.max, max_iter)

na_ind <- which(is.na(pred_vals),arr.ind=TRUE) # yikes, sorry berk
pred_vals[na_ind] <- Pmeans[ ratings[ S[[i]][na_ind],2] ] # fill in with column means (is this right??) 

# DONE --> do whatever you want with pred_vals now, big guy