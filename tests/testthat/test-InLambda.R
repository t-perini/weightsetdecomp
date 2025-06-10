test_that("InLambda for 2d vector", {
  v1=c(0.5,0.5)
  v2=c(-0.5,0.5)
  v3=c(0.6,0.6)
  expect_equal(c(InLambda(v1),InLambda(v2),InLambda(v3)), c(TRUE,FALSE,FALSE))
})

test_that("InLambda for 3d vector", {
  v1=c(0.5,0.5,0)
  v2=c(-0.5,0.5,0)
  v3=c(0.6,0.6,0.6)
  expect_equal(c(InLambda(v1),InLambda(v2),InLambda(v3)), c(TRUE,FALSE,FALSE))
})
