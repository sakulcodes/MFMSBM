CDMFM_new <- function(data, data1, niterations, beta.a, beta.b, GAMMA, LAMBDA, initNClusters)
{
  ## Model: A_{ij}|z,Q \sim Ber(Q_{z_i,z_j}) ##
  ##        Q_{rs} \sim Beta(beta.a,beta.b), r,s = 1,...,k ##
  ##        P(z_i = j) = \pi_j, j = 1,...,k ##
  ##        \pi \sim Dirichlet_k(GAMMA,...,GAMMA) ##
  ##        k-1 \sim possion(1) ##



  ################################################################

  ## Input: data = the adjacency matrix, a n by n matrix ##
  ##        data1 = the upper traiangle for the adjacency matrix, a n by n matrix ##
  ##        niterations = the total number of iterations in MFM-SBM ##
  ##        beta.a, beta.b = hyperparameters for the prior on elements in Q matrix in Beta distribution ##
  ##        GAMMA = the parameter in Dirichlet distribution that controls the relative size of clusters ##
  ##        LAMBDA = the parameter for Poisson distrition ##
  ##        initNClusters = the initial number of clusters ##


  ## Output:
  ##         zout = clustering configuration, a n by 1 vector##
  ##         Qout = probability matrix, a k by k matrix ##

  #################################################################
}
