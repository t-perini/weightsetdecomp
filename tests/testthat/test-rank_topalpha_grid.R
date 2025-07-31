test_that("multiplication works", {
  Lambda <- weight_set(stepsize=0.5)
  metrics <- data.frame('cost'=c(10,20,30,40,50), 'time'=c(5.9, 3.3, 2.5, 4.1, 1.8), 'risk'=c(1,4,3,2,5))
  Lambda <- rank_topalpha_grid(Lambda, 3, metrics)
  expect_equal(sum(as.character(Lambda$TopAlpha)=='1.2.3'), 3)
})
