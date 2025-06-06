test_that("rank_aggregation does not change the actual values", {
  Lambda <- weight_set(0.5)
  metrics <- data.frame('cost'=c(10,20,30,40), 'time'=c(5.9, 3.3, 2.5, 4.1), 'risk'=c(1,4,3,2))
  Lambda <- rank_aggregation_grid(Lambda,metrics)
  expect_true(all(Lambda$Item.Label == randomize_labels(Lambda$Item.Label)))
})

test_that("rank_aggregation does change the level order of factor", {
  Lambda <- weight_set(0.5)
  metrics <- data.frame('cost'=c(10,20,30,40), 'time'=c(5.9, 3.3, 2.5, 4.1), 'risk'=c(1,4,3,2))
  Lambda <- rank_aggregation_grid(Lambda,metrics)
  expect_false(all(levels(Lambda$Item.Label) == levels(randomize_labels(Lambda$Item.Label))))
})
