import pandas as pd
import numpy as np
import sklearn.mixture as sklm
import matplotlib.pyplot as plt

import myfunctions as mf

#sel_model = "m4"
#dat = pd.read_csv('../../../../objects/stage3/cfa/' + sel_model + '_pred.csv', index_col=0)
#dat = dat.values

dat = pd.read_csv('../mclust_sim/simdat.csv', index_col=0)

use_Bayesian = True

aics=[]
bics=[]
likelihood = -np.inf
likelihoods=[]
for i in range(1, 11):
        n_init = 5
        if use_Bayesian == True:
            print("Bayesian estimation. " + str(i))
            gmm = sklm.BayesianGaussianMixture(n_components = i,
                                               n_init = n_init,
                                               covariance_type = 'full')
        else:
            print("Non-Bayesian estimation. " + str(i))
            gmm = sklm.GaussianMixture(n_components = i,
                                       n_init = n_init,
                                       covariance_type = 'full')
        gmm.fit(dat)

        c_means = gmm.means_
        c_cov = gmm.covariances_
        c_weights = gmm.weights_
        likelihood = gmm.lower_bound_
        plabels = gmm.predict_proba(dat)
        if use_Bayesian == False:
            aic = gmm.aic(dat)
            bic = gmm.bic(dat)
        else:
            aic = 0
            bic = 0

        print(c_means)

        aics.append(aic)
        bics.append(bic)
        likelihoods.append(likelihood)

        likelihood_tmp = gmm.lower_bound_
        if likelihood_tmp > likelihood:
            likelihood = likelihood_tmp


print(bics)
print(likelihoods)

p_data = likelihoods
x_pos = np.arange(len(p_data)) + 1
p1 = plt.axes((0, 0, 1, 1))
p1.bar(x_pos, p_data, align='center', alpha=0.5)
plt.show()

p2 = plt.axes((0, 0, 1, 1))
p2.scatter(dat.values[:,0], dat.values[:,1], s=5, marker='o', color='#56B4E9', alpha=0.8)
mf.plot_ellipses(p2, gmm.weights_, gmm.means_, gmm.covariances_)
plt.show()

np.savetxt('out/means.csv', c_means, delimiter=",")
np.savetxt('out/weights.csv', c_weights, delimiter=",")
for n in range(c_means.shape[0]):
    np.savetxt('out/covars_c' + str(n) + '.csv', c_cov[n], delimiter=",")
