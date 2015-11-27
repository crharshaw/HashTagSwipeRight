#This cross_validates lambda values for submatrices
cross_val <- function(M, ratings, size, iter, lambda_seq, kfold) {
    
    rank.max=20
    MSE<-matrix(,length(lambda_seq),kfold)

    yh<-matrix(,length(lambda_seq),100)
    
    
    
    source("impute_single.R")
    
    for(j in 1:kfold){ 
        
        miss_sample<-sample(1:dim(ratings)[1], 100)
        miss_entry<-ratings[miss_sample, c(1,2)]
        M_cv = Incomplete(ratings[-miss_sample,1], ratings[-miss_sample,2], ratings[-miss_sample,3])
        mcount<-1
        for (lam in lambda_seq) {
            cat(lam)
            y<-impute_single(M_cv, miss_entry, size, iter, lam, rank.max)

            
            MSE[mcount, j]<-mean((y-ratings[miss_sample,3])^2)
            mcount<-mcount+1
        }
        
    }
    
    lam_best<-lambda_seq(which(MSE==min(MSE)))
    return(lam_best)
}







impute_single <- function(M, miss_entry, size, iter, lam, rank.max) {
    #     if (size<rank.max) {
    #         stop("Error: rank.max is larger than dimension of submatrix", call. = TRUE, domain = NULL)
    #     }
    #     
    #     geterrmessage()
    strt<-Sys.time()
    val<-matrix(,iter,1)
    n<-dim(M)[1]
    
    strt<-Sys.time()
    
    for (j in 1:iter) {
        #make sure to include row and column of miss_entry into submatrix
        rand_row<-c(sample(1:n, size), miss_entry[1]) 
        rand_col<-c(sample(1:n, size), miss_entry[2])
        Y=M[rand_row, rand_col] #Y is the random submatrix
        
        Y<-as(Y,"Incomplete")
        Y_hat=softImpute(Y, rank.max=rank.max, lambda=lam, type='als', maxit=200)
        val[j]<-impute(Y_hat, size+1, size+1)
    }
    cat(Sys.time()-strt)
    return(mean(abs(val)))    
}
