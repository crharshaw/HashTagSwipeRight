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
D[which(is.na(D),arr.ind=TRUE)] = Inf # now, distance is Inf where distance is undefined

# look at the distribution of distances to determine where to cut them off?
distance <- data.frame(d = D)
distance$lab <- 'distances'
ggplot(distance, aes(d, fill = lab)) + geom_density(alpha = 0.2) + labs(title="Distribution of Distances", x="Distances", y="Density")

# plot common versus distance for random choices
count = 0
num_points = 10000
common = rep(0,num_points)
d = rep(0,num_points)
while(count < num_points){
  # select random indices
  ind = sample(1:size, 2)
  i = min(ind)
  j = max(ind)
  
  # compute common entries and distance
  if(i!=j){
    com = length(intersect(which(rmat[i,]!=0, arr.ind=TRUE), which(rmat[j,]!=0, arr.ind=TRUE)))
    if(com!=0){
      count = count+1
      d[count] = D[size*(i-1) - i*(i-1)/2 + j-i]
      common[count] = com
    }
  }
}
plot(common, d, main='Common Ratings vs Distance', xlab='Commonly Rated Profiles', ylab='Distances', pch=19)

# see description of distance format returned by daisy here: 
#     https://stat.ethz.ch/R-manual/R-devel/library/cluster/html/dissimilarity.object.html 
#     ind = size*(i-1) - i*(i-1)/2 + j-i

# # not worth it -- this took over 2 hours to construct
# # build the common matrix
# C <- 
# foreach(i=1:(size-1), .combine='c') %:% 
#   foreach(j=(i+1):size, .combine='c') %dopar%
#     length(intersect(which(rmat[i,]!=0, arr.ind=TRUE), which(rmat[j,]!=0, arr.ind=TRUE)))

