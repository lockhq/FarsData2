library(testthat)
context("fars_map_state")

test_that("throws error if state name given instead of state number", {
  throws_error(fars_map_state("Texas",2014))
})
