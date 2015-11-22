
import numpy as np 
import scipy.sparse as sp 
from sklearn import neighbors
import csv

def initialize(dataFile, imputeFile, size):

	# sparse data matrix
	X = sp.dok_matrix((size,size))
	reader = csv.reader(dataFile)
	for row in reader:
		X[int(row[0]),int(row[1])] = float(row[2])

	# entries to impute
	impute_entries = []
	reader = csv.reader(imputeFile)
	for row in reader:
		impute_entries.append((int(row[0]), int(row[1])))

	# empty distance matrix
	D = sp.dok_matrix((size,size), dtype='float64, float64')

	return X, D, impute_entries

def knn_impute(X, D, k, c, impute_entries, ctrlStr='seq', num_val_row):

	# handle the control string of impute or sequential
	if ctrlStr == 'imp':
		pred = {}
	elif ctrlStr == 'seq':
		# reorder impute_entries by num_val_row

	else:
		raise AttributeError (ctrlStr + ' is not recognized')

	# for each entry that we need to impute
	for ind in impute_entries:

		# get row and column of entry
		i = ind[0]
		j = ind[1]

		# get candidates (rows that have same column ranked)
		candidates = X.getcol(j).nonzero()

		# nearest neighbors -- set distance=Inf, common=0
		nearest = np.zeros(k,2)
		nearest[:,0] = np.inf

		# for each candidate 
		for cand in candidates:

			# store by (min,max)
			a = min(i,cand)
			b = max(cand,i)

			# check if distance is already computed
			if D[a,b][0] != 0:
				d, com = D[a,b]
			else:

				# get common indices 
				A = set(X.getrow(i).nonzero())
				B = set(X.getrow(cand).nonzero())
				com_ind = A.intersection(B)
				com = len(com_ind)

				# compute distance
				d = 0
				for m in com_ind:
					d = d + (X[i,m] - X[cand,m])^2
				d = np.sqrt(d)

				# update distance matrix
				D[a,b] = d, com

			# get furthest neighbor
			m = np.max(nearest, axis=0)[0] # lowest distance

			# add if closer than furthest neighbor
			if com >= c:
				if (d == nearest[m,0] and c > nearest[m,1]) or d <= nearest[m,0]:
					nearest[m,0] = d
					nearest[m,1] = c 

		# warning if not enough neighbors
		if np.any(nearest[:,0]==np.inf):
			nearest = nearest[nearest[:,0]!=np.inf,:]
			print 'Warning -- not enough neighbors for ' + str(ind) 

		# estimate via weighted averageing
		imp = np.dot(nearest[:,0],nearest[:,1]) / np.sum(nearest[:,1])
		if ctrlStr == 'imp':
			pred[ind] = imp
		elif ctrlStr == 'seq':
			X[i,j] = imp

	# add imputed entries for regular imputation
	if ctrlStr == 'imp':
		X.update(pred)

	return X, D

def knn_iter(X, D, k, c, tol, max_iter=50, missing_entries, quiet=False):

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
			X_new[i,j] = np.mean(X_tmp[indices[i],j])

		# update stopping criteria
		diff = np.linalg.norm(X_old-X_new, 'fro')
		iter_num = iter_num + 1

		# give updates
		if not quiet:
			if iter_num > max_iter:
				print 'Reached limit of ' + str(max_iter) + ' iterations'
			if diff < tol:
				print 'Reached tolerance in ' + str(iter_num) + ' iterations'
	
	return X, D

def kfold_crossval(X, D, num_folds, K, C, ctrlStr, num_val_row):

	# produce k folds

	# initialize cv error
	cv_mse = np.empty( (len(K), len(C), num_folds) ) 

	for i in range(len(K)):
		k = K[i]
		for j in range(len(C)):
			c = C[j]

			for l in range(num_folds):

				fold = kfolds[l]

				# decide which method to run
				if ctrlStr == "seq":
					X, D = knn_impute(X, D, k, c, fold, ctrlStr, num_val_row):


				elif ctrlStr == "imp":
					X, D = knn_impute(X, D, k, c, fold, ctrlStr, num_val_row):
					
				elif ctrlStr == "iter":
					tol = 1e-7
					knn_iter(X, D, k, c, tol, max_iter=50, missing_entries, quiet=False)