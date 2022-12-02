# Adapting mixture of finite mixture (MFM) to the stochastic block model (SBM) Package

# Intended Use
Social network analysis is the process of investigating social structures through the use of networks and graph theory. It characterizes networked structures in terms of nodes and the ties, edges, or links that connect them. A fundamental problem in network analysis is clustering the nodes into groups which share a similar connectivity pattern. This R Package proposes a coherent probabilistic framework for simultaneous estimation of the number of communities and the community structure, adapting recently developed Bayesian nonparametric techniques to network models. There are four functions implemented in this R Package. Firstly, the **CDMFM_new** function computes the collapsed sampler for mixture of finite mixture to the stochastic block model (random graph model wih planted clusters) by initialization of clustering configuration and Gibb’s sampling. Secondly, the **loglike** function computes the log-likelihood related to the Jth observation. Furthermore, the **logmargs** function gets the collapsed sampler for MFMSBM called m(A_j). Lastly, the **getDahl** function summarizes the sample from the MCMC (Markov chain Monte Carlo).
 
# Installation Instructions
You can install the MFMSBM Package in R with
``` r
devtools::install_github("sakulcodes/MFMSBM")
```
Upon installation, please run 
``` r
library(MFMSBM)
```
to get access to the functions available.

# Contributing
If you have an idea to improve MFMSBM, considering forking the repo and creating a pull request or opening an issue.

# Example for Dolphin Data
``` r
## Installing the MFMSBM Package
devtools::install_github("sakulcodes/MFMSBM")
library(MFMSBM)

## generate the data
set.seed(33)
n = 100 ## number of observations
kk = 3 ## number of clusters
Z <- c(sample(1:kk, size = kk, replace = FALSE),
  sample(1:kk, size = n-kk, replace = TRUE,prob = c(1,1,1))) # clustering configuration
Z = Z[order(Z)]
theta <- matrix(0.1,kk,kk) ## off-diagonal value for Q matrix
diag(theta) = 0.6 ## diagonal value for Q matrix
A = matrix(0,n,n) ##the adjacency matrix
AAA = matrix(0,n,n) ##the upper traiangle for the adjacency matrix
for (i in 1:n){
  for (j in i:n){
    A[i,j] = rbinom(1,1,prob=theta[Z[i],Z[j]])
    A[j,i] = A[i,j]
    AAA[i,j] = A[i,j]
  }
}
diag(AAA) = 0 ## making it without-selfloop network
diag(A) = 0 ## making it without-selfloop networkk

## taking the data into the MFM-SBM algorithm
set.seed(1)
fit1 = CDMFM_new(data = A, data1 = AAA, niterations = 100, beta.a = 1, beta.b = 1, GAMMA=1, LAMBDA = 1, initNClusters = 9)
fit1$Iterates[[i]] #is a list of length two, which denotes the ith sample in MCMC output. 
fit1$Iterates[[i]][[1]] ##denotes the clustering configuration z in ith iteration.
fit1$Iterates[[i]][[2]] ##denotes the Q matrix in ith iteration.

## estimated configuration using Dahl's method, choosing first 50 iterations in MCMC as burn-in
result1 = getDahl(fit1, burn = 50)
result1[[1]] ##denotes the estimated clustering configuration.
result1[[2]] ##denotes the estimated Q matrix.

## dolphin data example
## loading the data
load("yourpathway/dolphindata.RData")
set.seed(3); 
fit_dol = CDMFM_new(data = A, data1 = AAA, niterations = 300, beta.a = 2, beta.b = 2, GAMMA=1,LAMBDA = 1,initNClusters=ceiling(runif(1,1,10)))

## estimated clustering configuration using Dahl's method, choosing first 100 iterations in MCMC as burn-in
result_dol = getDahl(fit_dol, burn = 100)
```

# References
Geng, J., Bhattacharya, A. and Pati, D., 2019. Probabilistic community detection with unknown number of communities. Journal of the American Statistical Association, 114(526), pp.893-905.

