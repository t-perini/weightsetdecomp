test_that("rank_aggregation does not change the actual values", {
  Lambda <- weight_set(0.1)
  metrics <- data.frame('cost'=sample(1:10), 'time'=sample(1:10), 'risk'=sample(1:10))
  Lambda <- rank_aggregation_grid(Lambda,metrics)
  expect_true(all(Lambda$Item.Label == randomize_labels(Lambda$Item.Label)))
})

test_that("rank_aggregation does change the level order of factor", {
  Lambda <- weight_set(0.1)
  metrics <- data.frame('cost'=sample(1:10), 'time'=sample(1:10), 'risk'=sample(1:10))
  Lambda <- rank_aggregation_grid(Lambda,metrics)
  expect_false(all(levels(Lambda$Item.Label) == levels(randomize_labels(Lambda$Item.Label))))
})
