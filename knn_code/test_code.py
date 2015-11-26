
import knn_impute as knn
from scipy.io import savemat, loadmat
import os.path

# initialize data
print 'Constructing the variables...'
data_file = '../data/ratings.csv'
impute_file = '../data/IDMap.csv'
var_file = '../data/knn_vars.mat'
rate_num_file = '../data/rate_num.csv'
size = 10000
X, D, impute_entries, num_val_row, num_val_col = knn.initialize(data_file, impute_file, rate_num_file, size)

# parameters
ctrlStr = 'imp'
k = 8
c = 5

# run an imputation and see what happens!
print 'Running the imputation!'
pred = knn.knn_impute(X, D, k, c, impute_entries, ctrlStr, num_val_row)

# print the predicted values
for pre in pred.keys():
	print pre + ': ' + pred[pre]