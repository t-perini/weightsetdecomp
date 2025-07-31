test_that("AddIfUnique works with existing row", {
  Lambda1 = weight_set(stepsize=0.5)
  Lambda2 = weight_set(stepsize=0.1)
  Added = AddIfUnique(Lambda1,Lambda2[1,])
  expect_equal(dim(Added)[1], dim(Lambda1)[1])
})

test_that("AddIfUnique works wiht unique row", {
  Lambda1 = weight_set(stepsize=0.5)
  Lambda2 = weight_set(stepsize=0.1)
  Added = AddIfUnique(Lambda1,Lambda2[2,])
  expect_equal(dim(Added)[1], dim(Lambda1)[1]+1)
})
