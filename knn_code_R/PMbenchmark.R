###############################
#script for Profile Mean Benchmark

library("Matrix")
library('Cluster')

# get data
rating_file = '../data/ratings.csv'
id_file = '../data/IDMap.csv'
dist_file = '../'
ratings = read.table(rating_file, header=TRUE,sep=",")
idmap = read.table(id_file, header=TRUE,sep=",")
size = 10000

# create sparse matrix
rmat = sparseMatrix(i=ratings[,1],j=ratings[,2],x=ratings[,3])

# create sparse distance matrix
rmat_na <- as.matrix(rmat) 
rmat_na[which(rmat_na==0)] = NA # rmat_na has NA where rating is unobserved
D = daisy(rmat_na) # D_na is a distance matrix with NA where distance is undefined

# see description of distance format returned by daisy here: 
#     https://stat.ethz.ch/R-manual/R-devel/library/cluster/html/dissimilarity.object.html 
#     ind = size*(i-1) - i*(i-1)/2 + j-i

# build the common matrix
C <- 
foreach(i=1:(size-1), .combine='c') %:% 
  foreach(j=(i+1):size, .combine='c') %dopar%
    length(intersect(which(rmat[i,]!=0, arr.ind=TRUE), which(rmat[j,]!=0, arr.ind=TRUE)))



                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               Pnum = apply(rmat!=0,2,sum)
Psum = apply(rmat,2,sum)
Pmeans = Psum / Pnum

Pred = Pmeans[idmap[,2]]

PMbenchmark = cbind(idmap[,3],Pred)
colnames(PMbenchmark) = c("ID","Prediction")
write.table(PMbenchmark,file="PMbenchmark.csv",quote=FALSE,sep=",",row.names=FALSE,col.names=TRUE)
