#' Function for getting the collapsed sampler for MFM-SBM m(Aj)
#'
#' @param clusterassign n by 1 vector of clustering configuration
#' @param data n by n adjacency  matrix
#' @param J observation index
#' @param beta.a  hyperparameters for the prior on elements in Q matrix in Beta distribution
#' @param beta.b hyperparameters for the prior on elements in Q matrix in Beta distribution
#'
#' @return m(Aj) i.e the collapsed sampler for MFM-SBM
#' @export
#'
#' @examples
logmargs <- function(clusterassign,data,J,beta.a,beta.b) #here J means Jth observation
{
  ################################################################

  ## Input: clusterassign = clustering configuration, a n by 1 vector ##
  ##        data = the adjacency matrix, a n by n matrix ##
  ##        J = observation index ##
  ##        n = number of observations ##
  ##        beta.a, beta.b = hyperparameters for the prior on elements in Q matrix in Beta distribution ##

  ## Output: m(A_j) in Algotithm 1 (collapsed sampler for MFM-SBM) ##

}
