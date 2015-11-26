
import numpy as np 
import scipy.sparse as sp 
from sklearn import neighbors
import csv
from random import shuffle

def initialize(data_file, impute_file, rate_num_file, size):

	# sparse data matrix
	X = sp.dok_matrix((size,size))
	datacsv = open(data_file, 'rb')
	reader = csv.reader(datacsv)
	header = reader.next()
	for row in reader:
		X[int(row[0])-1,int(row[1])-1] = float(row[2])

	# entries to impute
	impute_entries = []
	impute_csv = open(impute_file, 'rb')
	reader = csv.reader(impute_csv)
	header = reader.next()
	for row in reader:
		impute_entries.append((int(row[0])-1, int(row[1])-1))

	# empty distance matrix
	D = sp.dok_matrix((size,size), dtype='float64, float64')

	# number of values in each row / column
	num_val_row = np.empty(size)
	num_val_col = np.empty(size)
	i = 0
	rate_num_csv = open(rate_num_file, 'rb')
	reader = csv.reader(rate_num_csv)
	header = reader.next()
	for row in reader:
		num_val_row[i] = int(row[0])
		num_val_col[i] = int(row[1])
		i = i+1

	return X, D, impute_entries, num_val_row, num_val_col

def knn_impute(X, D, k, c, impute_entries, ctrlStr, num_val_row=None, quiet=False):

	# handle the control string of impute or sequential
	if ctrlStr == 'imp':
		pass
	elif ctrlStr == 'seq':
		# check hat num_val_row is provided
		if num_val_row == None:
			raise AttributeError ('num_val_row not supplied')
		
		# reorder impute_entries by num_val_row
		impute_entries = sorted(impute_entries, key=lambda x: (num_val_row[x[0]], num_val_col[x[1]]), reverse=True)

	else:
		raise AttributeError (ctrlStr + ' is not recognized')

	# initialize dictionary of predictions
	pred = {}

	# timing stuff
	checks = 20
	points = np.linspace(0, len(impute_entries),num=checks)
	percents = np.linspace(0, 1, num=checks)
	count = 0

	# for each entry that we need to impute
	for ind in impute_entries:

		# get row and column of entry
		i = ind[0]
		j = ind[1]

		# get candidates (rows that have same column ranked)
		candidates = X.getcol(j).nonzero()[0]

		# nearest neighbors -- set distance=Inf, common=0
		nearest = np.zeros((k,3))
		nearest[:,0] = np.inf

		# for each candidate 
		for cand in candidates:

			# timing updates
			if count >= points[0]:
				if not quiet:
					print 'At index ' + str(ind) + '...%2.2f%% finished' % percents[0]
				percents = percents[1:]
				points = points[1:]
			count = count + 1

			# store by (min,max)
			a = min(i,cand)
			b = max(cand,i)

			# check if distance is already computed
			if D[a,b] != 0:
				d, com = D[a,b]
				print('\tRe-access!')
			else:

				# get common indices 
				A = set(X.getrow(i).nonzero()[1])
				B = set(X.getrow(cand).nonzero()[1])
				com_ind = A.intersection(B)
				com = len(com_ind)

				# compute distance
				if com == 0:
					d = np.inf
				else:
					d = 0.0
					for m in com_ind:
						d = d + (X[i,m] - X[cand,m])**2
					d = np.sqrt(d/com)

				# update distance matrix
				D[a,b] = d, com

			# get furthest neighbor
			m = np.argmax(nearest, axis=0)[0] # lowest distance

			# add if closer than furthest neighbor
			if com >= c:
				if (d == nearest[m,0] and c > nearest[m,1]) or d < nearest[m,0]:
					nearest[m,0] = d
					nearest[m,1] = c 
					nearest[m,2] = X[cand,j]

		# warning if not enough neighbors
		if np.any(nearest[:,0]==np.inf):
			nearest = nearest[nearest[:,0]!=np.inf,:]
			print 'Warning -- not enough neighbors for ' + str(ind) 

		# estimate via weighted averageing
		zero_d = np.where(nearest[:,0]==0) # take care of the zero distance case
		if zero_d:
			imp = np.mean(nearest[zero_d,0])
		else:
			weights = 1 / (nearest[:,0] * np.sum(1/nearest[:,0]) )
			imp = np.dot(weights, nearest[:,2])
		pred[ind] = imp 
		if ctrlStr == 'seq':
			X[i,j] = imp

	# add imputed entries for regular imputation
	if ctrlStr == 'imp':
		X.update(pred)

	return pred

def knn_iter(X, D, k, c, tol, max_iter, impute_entries, quiet=False):

	# create dense copy of this matrix
	X_old = X.toarray()

	# initialize knn object, difference, and number of interations
	nbrs = neighbors.NearestNeighbor(n_neighbors=k)
	diff = np.inf
	iter_num = 0

	while (diff > tol) and (iter_num < max_iter) :

		# impute ALL missing entries with column means
		tmp = X_old
		for ind in missing_entries:
			i = ind[0]
			j = ind[1]
			tmp[i,j] = X_old.getcol(j).get_nnz().mean()

		# perform knn to fill missing entries
		X_new = tmp
		distances, indices = nbrs.kneighbors(X)

		# update missing with mean of nearest neighbors
		for ind in missing_entries:
			i = ind[0]
			j = ind[1]
			X_new[i,j] = np.mean(tmp[indices[i],j])

		# update stopping criteria
		diff = np.linalg.norm(X_old-X_new, 'fro')
		iter_num = iter_num + 1

	# give updates
	if not quiet:
		if iter_num > max_iter:
			print 'Reached limit of ' + str(max_iter) + ' iterations'
		if diff < tol:
			print 'Reached tolerance in ' + str(iter_num) + ' iterations'

	# store predictions
	pred = {}
	for ind in impute_entries:
		pred[ind] = X_new[ind]
	
	return pred

def gen_kfold(X, k):

	# observable data
	obs = X.nonzero()
	n_obs = len(obs)

	avail = []
	for i in range(n_obs):
		avail.append( (obs[0][i], obs[1][i]) )
	shuffle(avail)

	# different sizes of partitions
	size1 = np.floor(n_obs / k)
	size2 = np.floor(n_obs / k) + 1

	# initialize list of partitions
	S = []

	# assign partitions
	for i in range(k):
		if i <= (k - n_obs % k):
			S.append(avail[:size1])
			avail = avail[size1:]
		else:
			S.append(avail[:size2])
			avail = avail[size2:]

	return S

def kfold_crossval(X, D, num_folds, K, C, ctrlStr, num_val_row):

	# produce k folds
	S = gen_kfold(X,num_folds)

	# initialize cv error
	cv_mse = np.empty( (len(K), len(C), num_folds) ) 

	for i in range(len(K)):
		k = K[i]
		for j in range(len(C)):
			c = C[j]

			for m in range(num_folds):
				fold = S[m]

				# decide which method to run
				if ctrlStr == "seq":
					pred = knn_impute(X.todok(copy=True), D, k, c, fold, ctrlStr, num_val_row)

				elif ctrlStr == "imp":
					pred = knn_impute(X.todok(copy=True), D, k, c, fold, ctrlStr)
					
				elif ctrlStr == "iter":
					tol = 1e-7
					max_iter = 50
					pred = knn_iter(X.todok(copy=True), D, k, c, tol, max_iter, fold)

				# compute the MSE 
				mse = 0
				for ind in fold:
					mse = mse + (pred[ind] - X[ind])^2
				cv_mse[i,j,m] = mse / len(fold)

	mean_mse = np.mean(cv_mse, axis=2)
	sd_mse = np.std(cv_mse, axis=2)

	min_k = np.min(mean_mse,axis=0)
	min_c = np.min(mean_mse,axis=1)

	return min_k, min_c, cv_mse
