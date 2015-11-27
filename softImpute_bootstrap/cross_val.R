#This cross_validates lambda values for submatrices
cross_val <- function(M, ratings, size, iter, lambda_seq, kfold) {
    
    rank.max=20
    MSE<-matrix(,length(lambda_seq),kfold)
    #I do't think that this is actually used...
    #yh<-matrix(,length(lambda_seq),100)
    
    
    
    source("impute_single.R")
    
    for(j in 1:kfold){ 
        cat("K=",j)
        miss_sample<-sample(1:dim(ratings)[1], 100)
        miss_entry<-ratings[miss_sample, c(1,2)]
        M_cv = Incomplete(ratings[-miss_sample,1], ratings[-miss_sample,2], ratings[-miss_sample,3])
        mcount<-1
        miss_entry<-as.matrix(miss_entry)
        for (lam in lambda_seq) {
            cat("lambda=",lam,"\n")
            y<-impute_single(M_cv, miss_entry, size, iter, lam, rank.max)

            
            MSE[mcount, j]<-mean((y-ratings[miss_sample,3])^2)
            mcount<-mcount+1
        }
        
    }
    MSE=apply(MSE,1, mean)
    
    lam_best<-lambda_seq[which(MSE==min(MSE))]
    #2.25056
    
    
#     Without weights
#     min(MSE)
#     [1] 4.897548
    
    return(lam_best)
}

