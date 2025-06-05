test_that("flex_rank returns proper order", {
  expect_equal( flex_rank(c(5.3, 6.8, 2.1, 2.1)), c(3,4,1,2) )
})

test_that("flex_rank works with ties", {
  expect_equal( flex_rank(c(5.3, 6.8, 2.1, 2.1),ties=TRUE), c(3,4,1,1) )
})

