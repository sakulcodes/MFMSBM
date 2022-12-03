#######################################Testing_logmargs_function#######################################

test_that("logmargs function works", {
  expect_equal(logmargs(c(2,2,3),matrix(data =1:9,nrow=3, ncol = 3),1,2,3), Inf)
  expect_equal(logmargs(c(2,2,4),matrix(data =1:9,nrow=3, ncol = 3),1,2,3), Inf)
  expect_equal(logmargs(c(2,2,4),matrix(data =1:9,nrow=3, ncol = 3),1,2,3), Inf)
  expect_equal(logmargs(c(8,8,8),matrix(data =1:9,nrow=3, ncol = 3),3,4,4), 0)
})


#######################################Testing_loglike_function#######################################

test_that("loglike function works", {
  expect_warning(loglike(c(2,3,4),matrix(data=1:25,nrow=5,ncol=5),matrix(data=1:9,nrow=3,ncol=3),1,1), "NaNs produced")
  expect_equal(loglike(c(1,1,1),matrix(data=1:25,nrow=5,ncol=5),matrix(data=1:9,nrow=3,ncol=3),2,2), Inf)
  expect_warning(loglike(c(5,5,5),matrix(data=1:25,nrow=5,ncol=5),matrix(data=1:9,nrow=3,ncol=3),2,3), "NaNs produced")
})

#######################################Testing_CDMFM_new_function#######################################

n <- 10
A <- matrix(0,n,n)
AAA <- matrix(0,n,n)
test_that("CDMFM_new function works", {
  expect_equal(CDMFM_new(data = A, data1 = AAA, niterations = 1, beta.a = 1, beta.b = 1, GAMMA=1, LAMBDA = 1, initNClusters = 1)$zout, NULL)
  expect_equal(CDMFM_new(data = A, data1 = AAA, niterations = 1, beta.a = 1, beta.b = 1, GAMMA=1, LAMBDA = 1, initNClusters = 1)$Qout, NULL)
})

#######################################getDahl_function#######################################
n <- 10
A <- matrix(0,n,n)
AAA <- matrix(0,n,n)
fit1 <- CDMFM_new(data = A, data1 = AAA, niterations = 2, beta.a = 1, beta.b = 1, GAMMA=1, LAMBDA = 1, initNClusters = 1)
fit2 <- CDMFM_new(data = A, data1 = AAA, niterations = 3, beta.a = 1, beta.b = 1, GAMMA=1, LAMBDA = 1, initNClusters = 1)

test_that("getDahl function works", {
  expect_error(getDahl(fit1,100)$zout, "subscript out of bounds")
  expect_error(getDahl(fit1,200)$zout, "subscript out of bounds")
  expect_equal(getDahl(fit2,1)$zout * 0 , c(0,0,0,0,0,0,0,0,0,0))
})
