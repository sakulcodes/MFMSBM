#' Log-likelihood function related to Jth observation
#'
#' @param clusterassign n by 1 vector of cluster configuration
#' @param param k by k probability matrix
#' @param data n by n adjacency matrix
#' @param J observation index
#' @param n number of observations
#'
#' @return log-likelihood related to the jth observation
#' @export
#'
#' @examples
loglike <- function(clusterassign,param,data,J,n) #here J means Jth observation
{
  clustersize = max(clusterassign)
  param = as.matrix(param)

  if (J==1) {result2 = 0
  for (ii in c((J+1):n))
  {
    result2 = result2 + data[J,ii]*log(param[clusterassign[J],clusterassign[ii]])+(1-data[J,ii])*log(1-param[clusterassign[J],clusterassign[ii]])
  }
  output = sum(result2)} else if (J==n){
    result = 0
    for (ii in c(1:(J-1)))
    {
      result = result + data[ii,J]*log(param[clusterassign[ii],clusterassign[J]])+(1-data[ii,J])*log(1-param[clusterassign[ii],clusterassign[J]])
    }
    output = sum(result)
  } else {
    result = 0
    for (ii in c(1:(J-1)))
    {
      result = result + data[ii,J]*log(param[clusterassign[ii],clusterassign[J]])+(1-data[ii,J])*log(1-param[clusterassign[ii],clusterassign[J]])
    }

    result2 = 0
    for (ii in c((J+1):n))

    {
      result2 = result2 + data[J,ii]*log(param[clusterassign[J],clusterassign[ii]])+(1-data[J,ii])*log(1-param[clusterassign[J],clusterassign[ii]])
    }
    output = sum(result)+sum(result2)}
  output
}
