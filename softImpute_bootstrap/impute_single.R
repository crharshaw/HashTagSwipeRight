impute_single <- function(M, miss_entry, size, iter, lam, rank.max) {
    
    scaling=TRUE
    
    #This give weights for sample in order to make the matrix less sparse
    a=table(ratings[,1])
    row_prob=a/10000
    
    b=table(ratings[,2])
    col_prob=b/10000

    val<-matrix(,iter,dim(miss_entry)[1])
    full_vals<-matrix(,iter,1)
    n<-dim(M)[1]
    
    strt<-Sys.time()
    
    ind_vec<-(size+1):(size+100) #these are the indices of the missing values to pull out
    ind_mat<-matrix(rep(ind_vec,2), ncol=2)
    
    for (j in 1:iter) {
        #Weighted samples are not used
        #rand_row<-c(sample(1:n, size, prob=row_prob), miss_entry[,1]) 
        #rand_col<-c(sample(1:n, size, prob=col_prob), miss_entry[,2])
        rand_row<-c(sample(1:n, size), miss_entry[,1]) 
        rand_col<-c(sample(1:n, size), miss_entry[,2])
 
        #CHANGE M_cv BACK TO M
        Y<-M[rand_row, rand_col] #Y is the random submatrix
        if (scaling) {
            full_num<-which(Y!=0, arr.ind=TRUE)
            mat_mean<-mean(Y[full_num])
            Y[full_num]<-Y[full_num]-mat_mean
        }

        full_vals[j]<-length(which(Y!=0))/dim(Y)[1]^2
        

        Y<-as(Y,"Incomplete")
        Y_hat=softImpute(Y, rank.max=rank.max, lambda=lam, type='als', maxit=300)
        #Y_comp=complete(Y, Y_hat)
        #cat(length(impute(Y_hat, ind_mat[,1], ind_mat[,2])+mat_mean))
        #cat(length(val[j,]))
        val[j,]<-impute(Y_hat, ind_mat[,1], ind_mat[,2])+mat_mean
    }
    Sys.time()-strt
    
    #4.818132 secs for iter=5, size=100, matrix is 200x200
    
    #3.15855 seconds for iter=5, size=50 but matrix is 100*100 because of 50 
    #missing values
    
    
    #(500000/100)*2.5/60/60=4.17 hours, this is if 100 by 100 matrix is used
    
    #20 lambda's, 4 folds
    
    preds<-apply(val,2,mean)
    preds[preds>10]=10
    preds[preds<1]=1
    
    return(preds)    
}
