test_that("weight_set() generates correct size of df", {
  expect_equal(dim(weight_set(0.5)), c(6,5))
})

test_that("weight_set() has every row sum = 1", {
  df = weight_set(0.1)
  df$sum = 1 - (df$lambda1 + df$lambda2 + df$lambda3)
  expect_lt(sum(df$sum), 0.001)
})
