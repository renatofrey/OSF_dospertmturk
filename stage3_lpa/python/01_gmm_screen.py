import pandas as pd
import numpy as np
import sklearn.mixture as sklm
import matplotlib.pyplot as plt
import sys
import myfunctions as mf

if len(sys.argv) == 1:
    s = 12345
else:
    s = int(sys.argv[1])

print("Running GMM estimation with seed: " + str(s))

sel_model = "m5"
dat = pd.read_csv('../../objects/stage3/cfa/' + sel_model + '_pred.csv', index_col=0)

use_Bayesian = True

aics=[]
bics=[]
likelihood = -np.inf
likelihoods=[]
bics=[]
for i in range(1,51):
        n_init = 1
        if use_Bayesian == True:
            print("Bayesian estimation of " + str(i) + " clusters.")
            gmm = sklm.BayesianGaussianMixture(n_components = i,
                                               n_init = n_init,
                                               covariance_type = 'full',
                                               random_state = s)
        else:
            print("Non-Bayesian estimation of " + str(i) + " clusters.")
            gmm = sklm.GaussianMixture(n_components = i,
                                       n_init = n_init,
                                       covariance_type = 'full')

        gmm.fit(dat)

        c_means = gmm.means_
        c_cov = gmm.covariances_
        c_weights = gmm.weights_
        likelihood = gmm.lower_bound_
        plabels = gmm.predict_proba(dat)

        # get BIC
        n_features = gmm.means_.shape[1]
        cov_params = gmm.n_components * n_features * (n_features + 1) / 2
        mean_params = n_features * gmm.n_components
        npars = cov_params + mean_params + gmm.n_components - 1
        bic = (-2 * gmm.score(dat) * dat.shape[0] + npars * np.log(dat.shape[0]))

#        if use_Bayesian == True:
#            aic = 0
#            bic = 0
#        else:
#            aic = gmm.aic(dat)
#            bic = gmm.bic(dat)

#        print(c_means)

#        p_data = c_weights
#        x_pos = np.arange(len(p_data)) + 1
#        p1 = plt.axes((0, 0, 1, 1))
#        p1.bar(x_pos, p_data, align='center', alpha=0.5)
#        plt.show()
#
#        p2 = plt.axes((0, 0, 1, 1))
#        p2.scatter(dat.values[:,0], dat.values[:,1], s=5, marker='o', color='#56B4E9', alpha=0.8)

#        mf.plot_ellipses(p2, gmm.weights_, gmm.means_, gmm.covariances_)
#        plt.show()

#        aics.append(aic)

        bics.append(bic)
        likelihoods.append(likelihood)

infocrit = pd.DataFrame(
        {'lls': likelihoods,
         'bics': bics
         })
infocrit.to_csv(r'../../objects/stage3/lpa/py_gmm_screen/' + sel_model + "/" + f"{s:05d}" + '.csv', index = None, header=True)

print(infocrit)


#p_data = likelihoods
#x_pos = np.arange(len(p_data)) + 1
#p1 = plt.axes((0, 0, 1, 1))
#p1.bar(x_pos, p_data, align='center', alpha=0.5)
#plt.show()
#
#p2 = plt.axes((0, 0, 1, 1))
#p2.scatter(dat.values[:,0], dat.values[:,1], s=5, marker='o', color='#56B4E9', alpha=0.8)
#mf.plot_ellipses(p2, gmm.weights_, gmm.means_, gmm.covariances_)
#plt.show()

#np.savetxt('../objects/stage3/python/gmms/' + sel_model + "/" + f"{s:05d}" + '.csv', likelihoods, delimiter=",")
#np.savetxt('out/means.csv', c_means, delimiter=",")
#np.savetxt('out/weights.csv', c_weights, delimiter=",")
#for n in range(c_means.shape[0]):
#    np.savetxt('out/covars_c' + str(n) + '.csv', c_cov[n], delimiter=",")
