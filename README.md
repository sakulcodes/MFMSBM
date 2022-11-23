# Adapting mixture of finite mixture (MFM) to the stochastic block model (SBM) Package

# Intended Use
Social network analysis is the process of investigating social structures through the use of net- works and graph theory. It characterizes networked structures in terms of nodes and the ties, edges, or links that connect them. A fundamental problem in network analysis is clustering the nodes into groups which share a similar connectivity pattern. This R Package proposes a coherent probabilistic framework for simultaneous estimation of the number of communities and the community structure, adapting recently developed Bayesian nonparametric techniques to network models.


There are four functions implemented in this R Package. Firstly, the **CDMFM_new** function computes the collapsed sampler for mixture of finite mixture to the stochastic block model (random graph model wih planted clusters) by initialization of clustering configuration and Gibb’s sampling. Secondly, the **loglike** function takes in parameters: probability matrix(k by k matrix), adjacency matrix(n by n), observation index(jth), and number of observations(n), and computes the log-likelihood related to the Jth observation. Furthermore, the **logmargs** function takes in parameters: clustering configuration (n by 1), adjacency matrix(n by n), observation index(jth), number of observations(n), and the hyperparameters (beta.a, beta.b) for the prior on elements in Q matrix in Beta distribution to get the collapsed sampler for MFMSBM called m(A_j). Lastly, the **getDahl** function summarizes the sample from the MCMC (Markov chain Monte Carlo) by taking in two parameters: the result from the CDFM new and the number of burn-in iterations and returns the estimated clustering configuration(n by 1 vector) and estimated probability matrix(k by k).
 
# Installation Instructions
Below are the installation instructions to use this package:
1. loglike, logmargs, CDMFM_new and getDahl are the functions required for this algorithm. Load these functions first to get started.
2. Make your network data into adjacency matrix form.
3. To to have an undirected graph without self loops, make the adjacency matrix (denoted as "A") symmetric and all diagonal terms zero. 
4. For the adjacency maatrix above, make an upper triangular matrix (denoted by "AAA").
5. Decide the number of MCMC iterations, initial number of clusters, and values for all hyperparameters in the model including: beta.a, beta.b, GAMMA and LAMBDA.
6. Run CDMFM_new function and save the output to a list, denoted by "fit1".
7. Decide the number of burn-in iterations and run getDahl function to have the estimated clustering configuration and Q matrix.

# Things left to do
1. Integrate C++ into some of the functions like CDMFM_new and getDahl.R to make the computing faster.
2. Provide an example: the result of MFMSBM in dolphin social network (this will replicate the results in the paper). 
3. Do automatic testing for different test cases.
