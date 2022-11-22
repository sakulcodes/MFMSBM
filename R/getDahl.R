#' Dahl's method to summarize the samples from the MCMC
#'
#' @param MFMfit the result from CDMFM_new function
#' @param burn the number of burn-in interations
#'
#' @return zout = estimated clustering configuration, a n by 1 vector
#' @return Qout = estimated probability matrix, a k by k matrix
#' @export
#'
#' @examples
getDahl <- function(MFMfit, burn)
{
  iters <- MFMfit$Iterates[-(1:burn)]
  n <- length(iters[[1]][[1]])
  niters <- length(iters)
  membershipMatrices <- lapply(iters, function(x){
    clusterAssign <- x[[1]]
    outer(clusterAssign, clusterAssign, FUN = "==")
  })
  membershipAverage <- Reduce("+", membershipMatrices)/niters
  SqError <- sapply(membershipMatrices, function(x, av) sum((x - av)^2),
                    av = membershipAverage)
  DahlIndex <- which.min(SqError)
  DahlAns <- iters[[DahlIndex]]
  attr(DahlAns, "iterIndex") <- burn + DahlIndex
  attr(DahlAns, "burnin") <- burn
  DahlAns
}
