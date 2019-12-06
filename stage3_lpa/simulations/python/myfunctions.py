import numpy as np
import matplotlib as mpl
from sklearn.neighbors import KDTree

def avg_shortest_dist(X, N_samples):
    '''Calculate the average distance of he nearest neighbor of a randomly sampled point.
    IN:
    - X, arr, shape = samples x features
    - N_samples, int, how many points to sample randomly and get their nn
    OUT:
    - d_mu, float, average distance to nearest neighbor
    '''
    N = len(X)
    kdt = KDTree(X, metric='euclidean')

    list_d = []
    for i in range(N_samples):
        i1 = np.random.choice(N,replace=False,size=1)
        x1 = X[i1,:]
        d = kdt.query(x1, k=2, return_distance=True)[0][0][1]
        list_d += [d]
    return np.mean(list_d)


def data_randomize(arr_p_d_c_,m_replace = False):
	'''Randomize a dataset (with or without replacement) by all entries in a given dimension.
    (i.e. we randomize but keep the marginal distributions const)
    IN:
    - arr_p_d_c_, arr, shape: samples x features
    - m_replace, randomize with or without replacement (default=False)
    OUT:
    - arr_p_d_c_rand_, arr, shape: samples x features; randomized version of original dataset
	'''
	N_d_ = len(arr_p_d_c_[0,:])
	N_p_ = len(arr_p_d_c_[:,0])
	arr_p_d_c_rand_ = 0.0*arr_p_d_c_
	for i_d in range(N_d_):
		p_ = 1.0*arr_p_d_c_[:,i_d]
		if m_replace == False:
			np.random.shuffle(p_)
		else:
			p_ = np.random.choice(p_,size=len(p_),replace=True)
		arr_p_d_c_rand_[:,i_d] = p_
	return arr_p_d_c_rand_


def plot_ellipses(ax, weights, means, covars):
    for n in range(means.shape[0]):
        eig_vals, eig_vecs = np.linalg.eigh(covars[n])
        unit_eig_vec = eig_vecs[0] / np.linalg.norm(eig_vecs[0])
        angle = np.arctan2(unit_eig_vec[1], unit_eig_vec[0])
        # Ellipse needs degrees
        angle = 180 * angle / np.pi
        # eigenvector normalization
        eig_vals = 2 * np.sqrt(2) * np.sqrt(eig_vals)
        ell = mpl.patches.Ellipse(means[n], eig_vals[0], eig_vals[1],
                                  180 + angle, edgecolor='black')
        ell.set_clip_box(ax.bbox)
        ell.set_alpha(weights[n])
        #ell.set_alpha(0.2)
        #ell.set_facecolor('#56B4E9')
        ell.set_facecolor('#eb31e3')
        ax.add_artist(ell)