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
  clustersize = max(clusterassign)-1
  result = NULL
  for (ii in 1:clustersize)
  {
    sumA =  sum(data[J,which(clusterassign==ii)[which(clusterassign==ii)>J]]) + sum(data[which(clusterassign==ii)[which(clusterassign==ii)<J],J])
    S = length(which(clusterassign==ii)[which(clusterassign==ii)>J]) + length(which(clusterassign==ii)[which(clusterassign==ii)<J])
    result[ii] = lbeta(sumA+beta.a,S-sumA+beta.b)-lbeta(beta.a,beta.b)
  }
  sum(result)
}
