#######################################Testing_logmargs_function#######################################

test_that("logmargs function works", {
  cluster
  expect_equal(logmargs(c(2,2,3),matrix(data =1:9,nrow=3, ncol = 3),1,2,3), Inf)
  expect_equal(logmargs(c(2,2,4),matrix(data =1:9,nrow=3, ncol = 3),1,2,3), Inf)
  expect_equal(logmargs(c(2,2,4),matrix(data =1:9,nrow=3, ncol = 3),1,2,3), NaN)
  expect_equal(logmargs(c(8,8,8),matrix(data =1:9,nrow=3, ncol = 3),3,4,4), 0)
})

#######################################Testing_loglike_function#######################################

test_that("loglike function works", {
  expect_equal(loglike(c(2,3,4),matrix(data=1:25,nrow=5,ncol=5),matrix(data=1:9,nrow=3,ncol=3),1,1), NaN)
  expect_equal(loglike(c(1,1,1),matrix(data=1:25,nrow=5,ncol=5),matrix(data=1:9,nrow=3,ncol=3),2,2), Inf)
  expect_equal(loglike(c(5,5,5),matrix(data=1:25,nrow=5,ncol=5),matrix(data=1:9,nrow=3,ncol=3),2,3), NaN)
})

#######################################Testing_CDMFM_new_function#######################################

n <- 10
A <- matrix(0,n,n)
AAA <- matrix(0,n,n)

test_that("CDMFM_new function works", {
  expect_equal(CDMFM_new(data = A, data1 = AAA, niterations = 1, beta.a = 1, beta.b = 1, GAMMA=1, LAMBDA = 1, initNClusters = 1)$zout, c(1,1,1,1,1,1,1,1,1))
})

#######################################getDahl_function#######################################
n <- 10
A <- matrix(0,n,n)
AAA <- matrix(0,n,n)
fit1 <- CDMFM_new(data = A, data1 = AAA, niterations = 2, beta.a = 1, beta.b = 1, GAMMA=1, LAMBDA = 1, initNClusters = 1)

test_that("getDahl function works", {
  expect_equal(getDahl(fit1,1)$zout, c(1,1,1,1,1,1,1,1,1))
  expect_equal(getDahl(fit1,1)$Qout, matrix(data=0.02431389,nrow = 1,ncol=1))
})
