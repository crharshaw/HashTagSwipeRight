require("Matrix")
library("softImpute")
source("impute_single.R")
source("cross_val.R")
library(foreach)
library(doParallel)
# Data
ratings = read.table("../data/ratings.csv",header=TRUE,sep=",")
nobs = dim(ratings)[1]
idmap = read.table("../data/IDMap.csv",header=TRUE,sep=",")
n = 10000

# create incomplete matrix
M = Incomplete(i=ratings[,1], j=ratings[,2], x=ratings[,3])

size=100
iter=5

# Generate max lambda for submatrices so we know what to use
test_num = 100
lam_max<-matrix(,test_num,1)
for (k in 1:test_num) {
    rand_row<-c(sample(1:n , size+100)) 
    rand_col<-c(sample(1:n, size+100))
    Y=M[rand_row, rand_col] #Y is the random submatrix
    
    full_num<-which(Y!=0, arr.ind=TRUE)
    mat_mean<-mean(Y[full_num])
    Y[full_num]<-Y[full_num]-mat_mean

    lam_max[k]<-lambda0(Y, maxit=200)
}

lambda_max<-min(lam_max)

lambda_num = 20
lambda_min = 1e-3
lambda_seq = rev(10^ seq(log10(lambda_min), log10(lambda_max), length.out=lambda_num))

kfold = 4

#Cross validate to get best lambda to use
lam_best<-cross_val(M, ratings, size, 15, lambda_seq, kfold)



#Once best lambda is found, calculate 100 of idmap entries at a time
count=1

rank.max=40
lam=lam_best

preds<-matrix(,dim(idmap)[1],1)

for (i in 1:(dim(idmap)[1]/100)) {
    strt<-Sys.time()
    cat(i, "\n")
    miss_entry<-idmap[count:(count+99),c(1:2)]
    
    y<-impute_single(M, miss_entry, size, iter, lam, rank.max)
    preds[count:(count+99)]<-y
    count=count+100
    cat(Sys.time()-strt, "\n")
}

