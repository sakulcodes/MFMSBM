# Adapting mixture of finite mixture (MFM) to the stochastic block model (SBM) Package

# Intended Use
Social network analysis is the process of investigating social structures through the use of networks and graph theory. It characterizes networked structures in terms of nodes and the ties, edges, or links that connect them. A fundamental problem in network analysis is clustering the nodes into groups which share a similar connectivity pattern. This R Package proposes a coherent probabilistic framework for simultaneous estimation of the number of communities and the community structure, adapting recently developed Bayesian nonparametric techniques to network models.


There are four functions implemented in this R Package. Firstly, the **CDMFM_new** function computes the collapsed sampler for mixture of finite mixture to the stochastic block model (random graph model wih planted clusters) by initialization of clustering configuration and Gibb’s sampling. Secondly, the **loglike** function takes in parameters: probability matrix(k by k matrix), adjacency matrix(n by n), observation index(jth), and number of observations(n), and computes the log-likelihood related to the Jth observation. Furthermore, the **logmargs** function takes in parameters: clustering configuration (n by 1), adjacency matrix(n by n), observation index(jth), number of observations(n), and the hyperparameters (beta.a, beta.b) for the prior on elements in Q matrix in Beta distribution to get the collapsed sampler for MFMSBM called m(A_j). Lastly, the **getDahl** function summarizes the sample from the MCMC (Markov chain Monte Carlo) by taking in two parameters: the result from the CDFM new and the number of burn-in iterations and returns the estimated clustering configuration(n by 1 vector) and estimated probability matrix(k by k).
 
# Installation Instructions
You can install the MFMSBM Package with
``` r
devtools::install_github("sakulcodes/MFMSBM")
```
Upon installation, please run 
``` r
library(MFMSBM)
```
to get access to the functions available.

# Things left to do
1. Complete the implementation of CDMFM.R function 
2. Integrate C++ into some of the functions like CDMFM_new and getDahl.R to make the computing faster.
3. Provide an example: the result of MFMSBM in dolphin social network (this will replicate the results in the paper). 
4. Do automatic testing for different test cases.
