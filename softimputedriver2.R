require("Matrix")
library("softImpute")
setwd("~/Desktop/STAT 444/Data Competition/")
source("lwc_helper.R")
source("impute_single.R")
source("cross_val.R")
library(foreach)
library(doParallel)
# Data
ratings = read.table("ratings.csv",header=TRUE,sep=",")
nobs = dim(ratings)[1]
idmap = read.table("IDMap.csv",header=TRUE,sep=",")
n = 10000

# create incomplete matrix
M = Incomplete(i=ratings[,1], j=ratings[,2], x=ratings[,3])

size=100
iter=5

# Generate max lambda for submatrices
lam_max<-matrix(,100,1)
for (k in 1:100) {
    rand_row<-c(sample(1:n , size)) 
    rand_col<-c(sample(1:n, size))
    Y=M[rand_row, rand_col] #Y is the random submatrix
    lam_max[k]<-lambda0(Y)
}

lambda_max<-min(lam_max)
lambda_max

# We have found that lambda_max for submatrices of 500 are around 150, but these
# larger lambdas generally do not work great so we will start our lambda sequence
# with 20 for now

#ALSO: for submarices of size 500, we are using a max.rank of 20


lambda_num = 20
lambda_min = 1e-3
lambda_seq = rev(10^ seq(log10(lambda_min), log10(lambda_max), length.out=lambda_num))

kfold = 4

lam_best<-cross_val(M, ratings, size, 15, lambda_seq, kfold)




# 
# #This cross_validates lambda values for submatrices
# cross_val <- function(M, ratings, size, iter, lamseq) {
#     MSE<-matrix(,length(lamseq),1)
#     mcount<-1
#     for (lam in lamseq) {
#         #generate 100 known values to check error
#         known<-sample(1:dim(ratings)[1], 100)
#         known_vals<-ratings[known,3]
#         
#         yh<-matrix(,length(100),1)
#         count=1
#         for (i in known) {
#             Mnew<-M
#             miss_entry<-c(ratings[i,1], ratings[i,2])
#             Mnew[ratings[i,1], ratings[i,2]]<-0
#             y<-impute_single(Mnew, miss_entry, size, iter, lam, 30)
#             yh[count]<-y
#             count<-count+1
#         }
#         
#         MSE[mcount]<-mean((yh-known_vals)^2)/length(known_vals)
#         mcount<-mcount+1
#     }
#     lam_best<-lamseq(which(MSE==min(MSE)))
#     return(lam_best)
# }
# 
# #This calculates soft impute algorithem for the missing entry (miss_entry), over
# #some number (iter) of random submatrices (of size size) for a specified lambda 
# #value
# impute_single <- function(M, miss_entry, size, iter, lam, rank.max) {
#     if (size<rank.max) {
#         stop("Error: rank.max is larger than dimension of submatrix", call. = TRUE, domain = NULL)
#     }
#     
#     geterrmessage()
#     
#     val=matrix(,iter,1)
#     n=dim(M)[1]
#     
#     for (i in 1:iter) {
#         #make sure to include row and column of miss_entry into submatrix
#         rand_row<-c(sample(1:n, size), miss_entry[1]) 
#         rand_col<-c(sample(1:n, size), miss_entry[2])
#         Y=M[rand_row, rand_col] #Y is the random submatrix
#         Y_hat=softImpute(Y, rank.max=rank.max, lambda=lam, type='als')
#         val[i]<-Y_hat[miss_entry[1], miss_entry[2]]
#     }
#     return(val)    
# }
