
#################################LOGMARGS FUNCTION################################################################################
#' Function for getting the collapsed sampler for MFM-SBM m(Aj)
#'
#' logmargs function takes in five parameters: clusterassign, data, J, beta.a, beta.b, and returns the collapsed sampler for MFMSBM(i.e m(Aj))
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
#' logmargs(c(2,2,3),matrix(data =1:9,nrow=3, ncol = 3),1,2,3)
#'

logmargs <- function(clusterassign,data,J,beta.a,beta.b) #here J means Jth observation
{
  clustersize = max(clusterassign)-1 #defining the clusterzize
  result = NULL #initializing the result as null for now
  for (ii in 1:clustersize)
  {
    sumA =  sum(data[J,which(clusterassign==ii)[which(clusterassign==ii)>J]]) + sum(data[which(clusterassign==ii)[which(clusterassign==ii)<J],J])
    S = length(which(clusterassign==ii)[which(clusterassign==ii)>J]) + length(which(clusterassign==ii)[which(clusterassign==ii)<J])
    result[ii] = lbeta(sumA+beta.a,S-sumA+beta.b)-lbeta(beta.a,beta.b) #adding to the result matrix with each iteration
  }
  sum(result) #summing over the result matrix (this is our collapsed sampler for MFMSBM)
}

#################################LOGLIKE FUNCTION################################################################################
#' Log-likelihood function related to Jth observation
#'
#' loglike function takes in five parameters: clusterassign, param, data, J, n, and returns log-likelihood related to the jth observation.
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
#' loglike(c(5,5,5),matrix(data=1:25,nrow=5,ncol=5),matrix(data=1:9,nrow=3,ncol=3),2,3)
#'

loglike <- function(clusterassign,param,data,J,n) #here J means Jth observation
{
  #initializing clustersize and param
  clustersize = max(clusterassign)
  param = as.matrix(param)

  #running loops to implement the loglikelihood formula
  if (J==1) {result2 = 0
  for (ii in c((J+1):n))
  {
    result2 = result2 + data[J,ii]*log(param[clusterassign[J],clusterassign[ii]])+(1-data[J,ii])*log(1-param[clusterassign[J],clusterassign[ii]])
  }
  output = sum(result2)} else if (J==n){
    result = 0 #initializing result as zero
    for (ii in c(1:(J-1)))
    {
      result = result + data[ii,J]*log(param[clusterassign[ii],clusterassign[J]])+(1-data[ii,J])*log(1-param[clusterassign[ii],clusterassign[J]])
    }
    output = sum(result)
  } else {
    result = 0 #initializing result as zero if the above for loop is not true
    for (ii in c(1:(J-1)))
    {
      result = result + data[ii,J]*log(param[clusterassign[ii],clusterassign[J]])+(1-data[ii,J])*log(1-param[clusterassign[ii],clusterassign[J]])
    }

    result2 = 0 ##initializing result as zero if the above for loop is not true
    for (ii in c((J+1):n))

    {
      result2 = result2 + data[J,ii]*log(param[clusterassign[J],clusterassign[ii]])+(1-data[J,ii])*log(1-param[clusterassign[J],clusterassign[ii]])
    }
    output = sum(result)+sum(result2)}
  output #the loglikelihood related to the jth observation
}

#################################CDMFM_new################################################################################

#' Collapsed sampler for MFM-SBM
#'
#' CDMFM_new function returns a list with zout (clustering configuration, n by 1 vector), and Qout (probability matrix, k by k)
#'
#' @param data a n by n adjancey matrix
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
#' n <- 10
#' A <- matrix(0,n,n)
#' AAA <- matrix(0,n,n)
#' CDMFM_new(data = A, data1 = AAA, niterations = 1, beta.a = 1, beta.b = 1, GAMMA=1, LAMBDA = 1, initNClusters = 9)
#'

CDMFM_new <- function(data, data1, niterations, beta.a, beta.b, GAMMA, LAMBDA, initNClusters)
{
  n = dim(data)[1]
  #precomputation for prespecified coefficient VN
  lambda <- LAMBDA
  gamma <- GAMMA
  N=n ## n is the number of oberservations
  VN<-0
  tmax = n+10
  for (t in 1:tmax)
  {
    r = log(0)
    for (k in t:500)
    {
      b = sum(log((k-t+1):k))-sum(log((k*gamma):(k*gamma+N-1))) + dpois(k-1, lambda, log = TRUE)
      m = max(b,r)
      r = log(exp(r-m) + exp(b-m)) + m
    }
    VN[t] = r
  }
  # initialization of clustering configuration
  clusterAssign <- c(sample(1:initNClusters, size = initNClusters, replace = FALSE),
                     sample(1:initNClusters, size = n-initNClusters, replace = TRUE))

  Q<-matrix(0, initNClusters,initNClusters)
  for (i in 1:initNClusters){
    for (j in i:initNClusters){
      Q[i,j] = rbeta(1,beta.a,beta.b)
      Q[j,i] = Q[i,j]
    }
  }
  History <- vector("list", niterations)

  ##start Gibb's sampling
  for (iter in 1:niterations)
  {
    ## update z ##
    clusterSizes = table(as.factor(clusterAssign))
    nClusters = length(clusterSizes)
    for (i in 1:n)
    { #determine whether ith component is a singleton
      cur.cluster.i = clusterAssign[i]
      if (clusterSizes[clusterAssign[i]] > 1){
        # not a singleton, have |C|+1 choices
        c.counts.noi = clusterSizes  #c.counts.noi corresponds to |C|
        c.counts.noi[clusterAssign[i]] = c.counts.noi[clusterAssign[i]] - 1
        #finding the probs for sampling process
        clusterProbs = sapply(1:nClusters, function(x) {
          clusterAssign_temp = clusterAssign
          clusterAssign_temp[i] = x
          (GAMMA+c.counts.noi[x])*exp(loglike(clusterAssign_temp,Q,data,i,n))
        })
        clusterAssign_1 = clusterAssign
        clusterAssign_1[i] = nClusters+1
        clusterProbs[nClusters+1]<-GAMMA*exp(logmargs(clusterAssign_1,data,i,beta.a,beta.b))*exp(VN[nClusters+1]-VN[nClusters])
        #choose the cluster number for ith observation
        cluster.i <- sample(1:(nClusters+1), size = 1,
                            prob = clusterProbs)
        clusterAssign[i] <- cluster.i

        if (cluster.i > nClusters)
        {
          QQ = matrix(0,nClusters+1,nClusters+1)
          QQ[1:nClusters,1:nClusters] = Q
          QQ[nClusters+1,1:(nClusters+1)] = rbeta(nClusters+1,beta.a,beta.b)
          QQ[1:(nClusters+1),nClusters+1] = QQ[nClusters+1,1:(nClusters+1)]
          Q = QQ
          clusterSizes <- table(as.factor(clusterAssign)) # sorts according to labels
          nClusters <- length(clusterSizes)} else
          {Q = Q
          clusterSizes <- table(as.factor(clusterAssign))
          nClusters <- length(clusterSizes)}
      } else {
        # a singleton, have |C| choices
        c.counts.noi = clusterSizes
        c.counts.noi[clusterAssign[i]] = c.counts.noi[clusterAssign[i]] - 1 - GAMMA# can offset the gamma adding later
        #finding the probs for sampling process
        clusterProbs = sapply(1:nClusters, function(x) {
          clusterAssign_temp = clusterAssign
          clusterAssign_temp[i] = x
          (GAMMA+c.counts.noi[x])*exp(loglike(clusterAssign_temp,Q,data,i,n))
        })
        clusterAssign_1 = clusterAssign
        clusterAssign_1[i] = nClusters+1
        clusterProbs[nClusters+1]<-GAMMA*exp(logmargs(clusterAssign_1,data,i,beta.a,beta.b))*exp(VN[nClusters]-VN[nClusters-1])
        #choose the cluster number for ith observation
        cluster.i <- sample(1:(nClusters+1), size = 1,
                            prob = clusterProbs)
        # remove the empty cluster
        if (cluster.i > nClusters)
        {      clusterAssign[i] <- cur.cluster.i #put the new cluster in the place of the only singleten one
        clusterSizes <- table(as.factor(clusterAssign)) # sorts according to labels
        } else
        {
          clusterAssign[i] <- cluster.i
          clusterAssign <- ifelse(clusterAssign > cur.cluster.i, clusterAssign-1, clusterAssign) # to delete the previous group index
          clusterSizes <- table(as.factor(clusterAssign))
          nClusters <- length(clusterSizes)
          if (nClusters > 1) {Q = Q[-cur.cluster.i,][,-cur.cluster.i]} else {Q = Q[-cur.cluster.i,][-cur.cluster.i]}}
      }
    }
    # end for loop over subjects i
    ## update Q ##
    Q = matrix(0, nClusters,nClusters)
    AA = matrix(0,nClusters,nClusters)
    NN = matrix(0,nClusters,nClusters)
    for (r in 1:nClusters){
      for (s in r:nClusters)
      {
        AA[r,s] = sum(data1[clusterAssign==r,clusterAssign==s]) + sum(data1[clusterAssign==s,clusterAssign==r]) - (r==s)*sum(data1[clusterAssign==s,clusterAssign==r])
        med = matrix(0,n,n)
        med[which(clusterAssign==r),which(clusterAssign==s)] = 1
        med1 = matrix(0,n,n)
        med1[which(clusterAssign==s),which(clusterAssign==r)] = 1
        NN[r,s] = sum(med*lower.tri(med)) + sum(med1*lower.tri(med1))-(r==s)*sum(med1*lower.tri(med1))
        Q[r,s] = rbeta(1,AA[r,s]+beta.a,NN[r,s]-AA[r,s]+beta.b)
        Q[s,r] = Q[r,s]
      }
    }
    History[[iter]] <- list(zout = clusterAssign,Qout = Q)
    cat(" iteration:", iter,"\n",clusterAssign,"\n")
  }# for loop over iterations

  list(Iterates = History)
}

#################################GET_DAHL_FUNCTION################################################################################
#' Dahl's method to summarize the samples from the MCMC
#'
#' getDahl function takes in two parameters: MFMfit, burn, and returns a list with zout(estimated clustering configuration, n by 1 vector), and Qout(estimated probability matrix, k by k)
#'
#' @param MFMfit the result from CDMFM_new function
#' @param burn the number of burn-in interations
#'
#' @return zout = estimated clustering configuration, a n by 1 vector
#' @return Qout = estimated probability matrix, a k by k matrix
#' @export
#'
#' @examples
#' n <- 10
#' A <- matrix(0,n,n)
#' AAA <- matrix(0,n,n)
#' fit1 <- CDMFM_new(data = A, data1 = AAA, niterations = 2, beta.a = 1, beta.b = 1, GAMMA=1, LAMBDA = 1, initNClusters = 1)
#' getDahl(fit1,1)
#'
#'

getDahl <- function(MFMfit, burn)
{
  iters <- MFMfit$Iterates[-(1:burn)] #defining the iters value
  n <- length(iters[[1]][[1]]) #defining the number of iterations
  niters <- length(iters)
  membershipMatrices <- lapply(iters, function(x){
    clusterAssign <- x[[1]]
    outer(clusterAssign, clusterAssign, FUN = "==")
  })
  membershipAverage <- Reduce("+", membershipMatrices)/niters #calculating the average here
  SqError <- sapply(membershipMatrices, function(x, av) sum((x - av)^2),
                    av = membershipAverage)
  DahlIndex <- which.min(SqError) #getting the DahlIndex
  DahlAns <- iters[[DahlIndex]]
  attr(DahlAns, "iterIndex") <- burn + DahlIndex
  attr(DahlAns, "burnin") <- burn
  DahlAns #the result(i.e the zout and qout)
}
