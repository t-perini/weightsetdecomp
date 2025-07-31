test_that("rank_aggregation does not change the actual values", {
  Lambda <- weight_set(stepsize=0.1)
  metrics <- data.frame('cost'=sample(1:10), 'time'=sample(1:10), 'risk'=sample(1:10))
  Lambda <- rank_aggregation_grid(3,Lambda,metrics)
  expect_true(all(Lambda$Rank.Label == randomize_labels(Lambda$Rank.Label,seed=1)))
})

test_that("rank_aggregation does change the level order of factor", {
  Lambda <- weight_set(stepsize=0.1)
  metrics <- data.frame('cost'=sample(1:10), 'time'=sample(1:10), 'risk'=sample(1:10))
  Lambda <- rank_aggregation_grid(3,Lambda,metrics)
  expect_false(all(levels(Lambda$Rank.Label) == levels(randomize_labels(Lambda$Rank.Label,seed=1))))
})
