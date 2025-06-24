test_that("lex_knapsack works", {
  rank.label = '4.6.3.1.1.4'
  costs <- c(1.0,  0.5,  2.1,  4.0,  4.7, 1.5)
  sol1 = lex_knapsack(rank.label,costs,budget=6)
  expect_true(all(sol1==c(1,2,4)))
  sol2 = lex_knapsack(rank.label,costs,budget=7)
  expect_true(all(sol2==c(2,3,4)))
  sol3 = lex_knapsack(rank.label,costs,budget=8)
  expect_true(all(sol3==c(1,2,3,4)))
})
