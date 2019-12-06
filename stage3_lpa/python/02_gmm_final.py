import pandas as pd
import numpy as np
import sklearn.mixture as sklm
import sklearn.neighbors as skln
import sklearn.model_selection as sklms
import matplotlib.pyplot as plt
import sys
import myfunctions as mf

if len(sys.argv) == 1:
    s = 12345
else:
    s = int(sys.argv[1])

print("Running GMM estimation with seed: " + str(s))

sel_model = "m5"
dat_full = pd.read_csv('../../objects/stage3/cfa/' + sel_model + '_pred.csv', index_col=0)

dat_A = dat_full[full.subsample == "A"]
dat_B = dat_full[full.subsample == "B"]

use_Bayesian = True


for sel in range(0,3):
    if (sel == 0):
        dat = dat_full
        sub = "full/"
    if (sel == 1):
        dat = dat_A
        sub = "A/"
    if (sel == 2):
        dat = dat_B
        sub = "B/"

    print(sub)
    
    i = 7
    n_init = 25
    if use_Bayesian == True:
        print("Bayesian estimation of " + str(i) + " clusters.")
        gmm = sklm.BayesianGaussianMixture(n_components = i,
                                           n_init = n_init,
                                           max_iter = 1000,
                                           covariance_type = 'full',
                                           random_state = s)
    else:
        print("Non-Bayesian estimation of " + str(i) + " clusters.")
        gmm = sklm.GaussianMixture(n_components = i,
                                   n_init = n_init,
                                   covariance_type = 'full')
    
    gmm.fit(dat)
    
    c_means = pd.DataFrame.from_records(gmm.means_)
    c_cov = pd.DataFrame.from_records(gmm.covariances_)
    c_weights = gmm.weights_
    likelihood = gmm.lower_bound_
    
    # get BIC
    n_features = gmm.means_.shape[1]
    cov_params = gmm.n_components * n_features * (n_features + 1) / 2
    mean_params = n_features * gmm.n_components
    npars = cov_params + mean_params + gmm.n_components - 1
    bic = (-2 * gmm.score(dat) * dat.shape[0] + npars * np.log(dat.shape[0]))
    
    infocrit = pd.DataFrame(
            {'likelihood': [likelihood],
             'bic': [bic]
             })
    
    # test for spurious clusters and determine cluster enrichment with permutation test
    
    print("Estimating density at " + str(i) + " cluster means...")
    
    bw1 = mf.avg_shortest_dist(dat.values, 10000)
    
    grid = sklms.GridSearchCV(estimator=skln.KernelDensity(),
                        param_grid={'bandwidth': np.linspace(0, 1.0, 51)},
                        cv=20, # 20-fold cross-validation
                        n_jobs=3,
                        verbose=True)
    grid.fit(dat.values)
    bw2 = grid.best_params_['bandwidth']
    print("Bandwidths: " + str(bw1) + " / " + str(bw2))
    
    bw = bw2
    print("Using bandwith of " + str(bw))
    
    kde = skln.KernelDensity(kernel='gaussian', bandwidth=bw, algorithm="auto").fit(dat.values)
    density = np.exp(kde.score_samples(c_means))
    
    print("Estimating respective densities in randomized datasets...")
    n_rep = 1000
    density_rand = np.zeros((n_rep, i))
    for i_nrep in range(n_rep):
        dat_rand = mf.data_randomize(dat.values)
        kde_rand = skln.KernelDensity(kernel='gaussian', bandwidth=bw).fit(dat_rand)
        density_rand[i_nrep,:] = np.exp(kde_rand.score_samples(c_means))
    
    pvals=[]
    enrich=[]
    for j in range(0,i):
        pvals.append(sum(density_rand[:,j] > density[j]) / density_rand.shape[0])
        enrich.append(density[j] / (sum(density_rand[:,j]) / density_rand.shape[0]))
    c_info = pd.DataFrame(
            {'pvals': pvals,
             'enrich': enrich,
             'weights': c_weights,
             })
    
    # predict cluster memberships
    part_class = gmm.predict(dat)
    part_p = pd.DataFrame.from_records(gmm.predict_proba(dat))
    
    
    # save cluster means (clusters x factor values)
    c_means.to_csv('../../objects/stage3/lpa/py_gmm_final/' + sub + sel_model + '_' + str(i) + '_c_means.csv', index = None, header=True)
    
    # save cluster info (clusters x pvals / enrichment)
    c_info.to_csv('../../objects/stage3/lpa/py_gmm_final/' + sub + sel_model + '_' + str(i) + '_c_info.csv', index = None, header=True)
    
    # save model info critiera (LL / BIC)
    infocrit.to_csv('../../objects/stage3/lpa/py_gmm_final/' + sub + sel_model + '_' + str(i) + '_infocrit.csv', index = None, header=True)
    
    # save participants' classification
    np.savetxt('../../objects/stage3/lpa/py_gmm_final/' + sub + sel_model + '_' + str(i) + '_part_class.csv', part_class, delimiter=",")
    
    # save participants' classification probabilities
    part_p.to_csv('../../objects/stage3/lpa/py_gmm_final/' + sub + sel_model + '_' + str(i) + '_part_p.csv', index = None, header=True)
