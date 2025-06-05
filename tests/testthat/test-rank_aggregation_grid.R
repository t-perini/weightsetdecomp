test_that("rank_aggregation_grid gives correct output", {
  Lambda <- weight_set(0.5)
  metrics <- data.frame('cost'=c(10,20,30,40), 'time'=c(5.9, 3.3, 2.5, 4.1), 'risk'=c(1,4,3,2))
  Lambda <- rank_aggregation_grid(Lambda,metrics)
  expect_equal(as.character(Lambda$Item.Label[6]), '3.2.4.1')
})
