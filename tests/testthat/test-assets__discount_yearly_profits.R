# Tests for discount_yearly_profits

testthat::test_that("discount_yearly_profits applies correct discounting to yearly data", {
  yearly_scenarios <- data.frame(
    asset = c("A1", "A1", "A1", "A1"),
    company = c("C1", "C1", "C1", "C1"),
    year = c(2025, 2026, 2025, 2026),
    scenario = c("baseline", "baseline", "shock", "shock"),
    revenue = c(1000, 1020, 970, 989),
    profit = c(100, 102, 97, 98.9)
  )

  result <- discount_yearly_profits(yearly_scenarios, discount_rate = 0.05, base_year = 2025)

  # Should add discounted profit columns
  expected_cols <- c(
    "asset", "company", "year", "scenario", "revenue", "profit",
    "discounted_profit", "discounted_net_profit"
  )
  testthat::expect_true(all(expected_cols %in% names(result)))
  testthat::expect_equal(nrow(result), nrow(yearly_scenarios))

  # Check discounting formula: discounted = profit / (1 + rate)^(year - base_year)
  # 2025: discount factor = 1, 2026: discount factor = 1.05
  expected_2025 <- yearly_scenarios$profit[yearly_scenarios$year == 2025]
  expected_2026 <- yearly_scenarios$profit[yearly_scenarios$year == 2026] / 1.05

  actual_2025 <- result$discounted_profit[result$year == 2025]
  actual_2026 <- result$discounted_profit[result$year == 2026]

  testthat::expect_equal(actual_2025, expected_2025, tolerance = 1e-8)
  testthat::expect_equal(actual_2026, expected_2026, tolerance = 1e-8)

  # discounted_net_profit should equal discounted_profit
  testthat::expect_equal(result$discounted_profit, result$discounted_net_profit)
})

testthat::test_that("discount_yearly_profits shows monotonicity with respect to discount rate", {
  yearly_scenarios <- data.frame(
    asset = c("A1", "A1"),
    company = c("C1", "C1"),
    year = c(2025, 2030),
    scenario = c("baseline", "baseline"),
    revenue = c(1000, 1200),
    profit = c(100, 120)
  )

  # Test different discount rates
  rates <- c(0.05, 0.10, 0.15)
  results <- list()

  for (i in seq_along(rates)) {
    results[[i]] <- discount_yearly_profits(yearly_scenarios, discount_rate = rates[i])
  }

  # Higher discount rate should yield lower total discounted profit
  for (i in 2:length(rates)) {
    prev_total <- sum(results[[i - 1]]$discounted_net_profit)
    curr_total <- sum(results[[i]]$discounted_net_profit)

    testthat::expect_true(curr_total < prev_total,
      info = paste(
        "Higher discount rate", rates[i],
        "should yield lower total discounted profit than", rates[i - 1]
      )
    )
  }
})

testthat::test_that("discount_yearly_profits zero rate identity", {
  yearly_scenarios <- data.frame(
    asset = "A1",
    company = "C1",
    year = 2025,
    scenario = "baseline",
    revenue = 1000,
    profit = 100
  )

  # Zero discount rate should leave values unchanged
  result <- discount_yearly_profits(yearly_scenarios, discount_rate = 0.0)

  testthat::expect_equal(result$discounted_net_profit, yearly_scenarios$profit, tolerance = 1e-10)
})

testthat::test_that("discount_yearly_profits handles different base years", {
  yearly_scenarios <- data.frame(
    asset = c("A1", "A1"),
    company = c("C1", "C1"),
    year = c(2025, 2030),
    scenario = c("baseline", "baseline"),
    revenue = c(1000, 1200),
    profit = c(100, 120)
  )

  # Test different base years
  result_2025 <- discount_yearly_profits(yearly_scenarios, discount_rate = 0.05, base_year = 2025)
  result_2030 <- discount_yearly_profits(yearly_scenarios, discount_rate = 0.05, base_year = 2030)

  # With base_year = 2030, the 2030 values should not be discounted
  testthat::expect_equal(result_2030$discounted_profit[result_2030$year == 2030], 120)

  # With base_year = 2025, the 2025 values should not be discounted
  testthat::expect_equal(result_2025$discounted_profit[result_2025$year == 2025], 100)
})

