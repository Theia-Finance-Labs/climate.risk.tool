testthat::test_that("list_hazard_inventory parses keys", {
  hz <- list(a = 1, b = 2)
  names(hz) <- c("floods__1in10", "heat__ssp245")
  inv <- list_hazard_inventory(hz)
  testthat::expect_true(all(c("key", "hazard_type", "scenario") %in% names(inv)))
  testthat::expect_equal(inv$hazard_type[1], "floods")
})


