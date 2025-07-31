test_that("equi_transform maintains all rows", {
  Lambda = weight_set(stepsize=0.05)
  expect_equal(dim(Lambda)[1], dim(equi_transform(Lambda))[1])
})

test_that("equi_transform has appropriate x range", {
  Lambda = equi_transform(weight_set(stepsize=0.05))
  expect_equal(range(Lambda$equilambda1), c(-0.5,0.5))
})

