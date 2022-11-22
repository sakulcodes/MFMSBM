# Adapting mixture of finite mixture (MFM) to the stochastic block model (SBM)

# Description
The file code_MFMSBM.R contains the codes needed for Collapsed sampler for MFM-SBM (c-MFM-SBM).

# Installation Instructions
Below are the steps you should follow to use these codes:
1. Load the functions required for this algorithm: loglike, logmargs, CDMFM_new and getDahl.
2. Make your network data into adjacency matrix form.
3. Make the adjacency matrix symmetric and all diagonal terms zero to have an undirected graph without self loops, denoted as "A".  
4. Make an upper triangular matrix for the adjacency matrix above, denoted by "AAA".
5. Decide the number of MCMC iterations, initial number of clusters, and values for all hyperparameters in the model including: beta.a, beta.b, GAMMA and LAMBDA. Definitions of those hyperparameters can be found in code_MFMSBM.R.
6. Run CDMFM_new function and save the output to a list, denoted by "fit1".
7. Decide the number of burn-in iterations and run getDahl function to have the estimated clustering configuration and Q matrix.

# Things left to do
