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
        #make sure to include row and column of miss_entry into submatrix
        rand_row<-c(sample(1:n, size, prob=row_prob), miss_entry[,1]) 
        rand_col<-c(sample(1:n, size, prob=col_prob), miss_entry[,2])
 
        #CHANGE M_cv BACK TO M
        Y<-M_cv[rand_row, rand_col] #Y is the random submatrix
        if (scaling) {
            full_num<-which(Y!=0, arr.ind=TRUE)
            mat_mean<-mean(Y[full_num[,1], full_num[,2]])
            mat_mean<-mean(Y[Y!=0])
            
            Y<-Y[full_num[,1], full_num[,2]]-mat_mean
        }
        mean(Y[Y!=0])
        
        full_vals[j]<-length(which(Y!=0))
        

        Y<-as(Y,"Incomplete")
        Y_hat=softImpute(Y, rank.max=rank.max, lambda=lam, type='als', maxit=200)
        #Y_comp=complete(Y, Y_hat)
        val[j,]<-impute(Y_hat, ind_mat[,1], ind_mat[,2])
    }
    Sys.time()-strt
    
    preds<-apply(val,2,mean)
    preds[preds>10]=10
    preds[preds<1]=1
    
    return(preds)    
}
