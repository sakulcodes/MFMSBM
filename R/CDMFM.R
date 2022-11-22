#' Collapsed sampler for MFM-SBM
#'
#'
#' @param datat a n by n adjancey matrix
#' @param data1 a n by n upper triangle for the adjacency matrix
#' @param niterations the total number of iterations in MFMSBM
#' @param beta.a hyperparameters for the prior on elements in Q matrix in Beta distribution
#' @param beta.b hyperparameters for the prior on elements in Q matrix in Beta distribution
#' @param GAMMA the parameter in Dirichlet distribution that controls the relative size of clusters
#' @param LAMBDA the parameter for Poisson distrition
#' @param initNClusters the initial number of clusters
#'
#' @return zout = clustering configuration, a n by 1 vector
#' @return Qout = probability matrix, a k by k matrix
#' @export
#'
#' @examples
#'
CDMFM_new <- function(data, data1, niterations, beta.a, beta.b, GAMMA, LAMBDA, initNClusters)
{
}
