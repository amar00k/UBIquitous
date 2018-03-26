library(UBIquitous)

test_that("gen_id returns unique ids", {
  ids <- sapply(1:100, function(i) gen_id())

  expect_equal(length(ids), 100)
  expect_equal(class(ids), "numeric")
  expect_true(all(duplicated(ids) == FALSE))
})

test_that("extract_params returns the right number of parameters", {
  par <- (function() extract_parameters())()
  expect_equal(length(par), 0)

  par <- (function(x) extract_parameters())()
  expect_equal(length(par), 1)

  par <- (function(x=2) extract_parameters())()
  expect_equal(length(par), 1)

  par <- (function(x, y=5) extract_parameters())()
  expect_equal(length(par), 2)

  # this is not the behaviour we want...
  par <- (function(x, y=5, ...) extract_parameters())(10, n=1, m=2)
  expect_equal(length(par), 3)
})


