# Team HashTagSwipeRight
# November 2015
#
# analytics.R 
# compute interesting results on the data set

library('gplots')
library('Matrix')

# get data
gender_file = '../data/gender.csv'
rating_file = '../data/ratings.csv'
id_file = '../data/IDMap.csv'
ratings = read.table(rating_file, header=TRUE, sep=",")
idmap = read.table(id_file, header=TRUE, sep=",")
gender = read.table(gender_file, header=TRUE, sep=",")

# create sparse matrix
rmat = sparseMatrix(i=ratings[,1],j=ratings[,2],x=ratings[,3])

# NUMBER OF RATINGS vs AVERAGE RATING
Pnum = apply(rmat!=0,2,sum)
Psum = apply(rmat,2,sum)
Pmeans = Psum / Pnum
correlation = cor(Pnum,Pmeans)
titleStr = sprintf("Number of Ratings vs Average Rating\nCorr=%.03f", correlation)
plot(Pnum, Pmeans, main=titleStr, xlab='Number of Ratings', ylab='Average Rating', pch=19)

ind = which(Pnum<=1000 & Pnum>=600,arr.ind=TRUE)
plot(Pnum[ind], Pmeans[ind], main='Number of ratings vs Average Rating', xlab='Number of Ratings', ylab='Average Rating', pch=19)


# VALUES OF RATINGS
ratings_total = rep(0,10)
ratings_male = rep(0,10)
ratings_female = rep(0,10)
rating_gender = gender[ratings[,1],2]
for(i in 1:10){
	ratings_total[i] = length(which(ratings[,3]==i))
	ratings_male[i] = length(which(ratings[,3]==i & rating_gender=='M'))
	ratings_female[i] = length(which(ratings[,3]==i & rating_gender=='F'))
}
ratings_total = ratings_total / sum(ratings_total)
ratings_male = ratings_male / sum(ratings_male)
ratings_female = ratings_female / sum(ratings_female)
r_t = c(ratings_total, ratings_male, ratings_female)
ylim = c(min(r_t), max(r_t))
plot(1:10,ratings_total, type='o', col='black', main="Distribution of Ratings", xlab="Rating", ylab="Count", ylim=ylim)
lines(1:10, ratings_male, type='o', col='red')
lines(1:10, ratings_female, type='o', col='blue')
smartlegend(x="right",y="top", c('Total','Male','Female'), lty=c(1,1,1), col=c('black','red','blue'))


#  NUMBER OF RATINGS
# DENSITY
num_rat_total = data.frame(num = apply(rmat!=0,1,sum))
num_rat_total$a <- 'total'
num_rat_male = data.frame(num = apply(rmat[which(gender[,2]=='M'),]!=0,1,sum))
num_rat_male$a <- 'male'
num_rat_female = data.frame(num = apply(rmat[which(gender[,2]=='F'),]!=0,1,sum))
num_rat_female$a <- 'female'
num_rat <- rbind(num_rat_total, num_rat_male, num_rat_female)

ggplot(num_rat, aes(num, fill = a)) + geom_density(alpha = 0.2) + labs(title="Number of Profiles Rated", x="Profiles Rated", y="Density")
ggplot(num_rat, aes(num, fill = a)) + geom_density(alpha = 0.2) + labs(title="Number of Profiles Rated", x="Profiles Rated", y="Density") + xlim(0,500)

# MALE RATINGS

