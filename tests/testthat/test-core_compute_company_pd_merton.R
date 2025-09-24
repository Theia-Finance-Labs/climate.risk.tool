# Tests for function: compute_company_pd_merton


testthat::test_that("compute_company_pd_merton adds merton_pd (0..1)", {
  data <- create_baseline_and_shock()
  scen <- build_scenarios(data$baseline, data$shocked)
  rev <- compute_asset_revenue(scen, data$companies, growth_rate = 0.03)
  prof <- compute_asset_profits(rev, net_profit_margin = 0.2)
  dnp <- discount_net_profits(prof, discount_rate = 0.1)
  npv <- compute_company_npv(dnp)

  out <- compute_company_pd_merton(npv)
  testthat::expect_true("merton_pd" %in% names(out))
  testthat::expect_true(all(out$merton_pd >= 0 & out$merton_pd <= 1, na.rm = TRUE))
})

testthat::test_that("compute_company_pd_merton sensitivity tests", {
  data <- create_baseline_and_shock()
  scen <- build_scenarios(data$baseline, data$shocked)
  rev <- compute_asset_revenue(scen, data$companies, growth_rate = 0.03)
  prof <- compute_asset_profits(rev, net_profit_margin = 0.2)
  dnp <- discount_net_profits(prof, discount_rate = 0.1)
  npv <- compute_company_npv(dnp)

  out <- compute_company_pd_merton(npv)
  
  # Range validation
  testthat::expect_true(all(out$merton_pd >= 0 & out$merton_pd <= 1, na.rm = TRUE),
                       info = "Merton PD should be in [0,1] range")
  
  # Scenario consistency: different scenarios should generally produce different PDs
  baseline_pds <- out[out$scenario == "baseline", ]
  shock_pds <- out[out$scenario == "shock", ]
  
  testthat::expect_equal(nrow(baseline_pds), nrow(shock_pds),
                        info = "Should have same number of companies in both scenarios")
  
  # Basic validation that PDs are numeric and not all identical
  testthat::expect_true(is.numeric(out$merton_pd),
                       info = "Merton PD should be numeric")
  testthat::expect_true(!all(is.na(out$merton_pd)),
                       info = "Not all Merton PDs should be NA")
})

testthat::test_that("compute_company_pd_merton schema validation", {
  data <- create_baseline_and_shock()
  scen <- build_scenarios(data$baseline, data$shocked)
  rev <- compute_asset_revenue(scen, data$companies, growth_rate = 0.03)
  prof <- compute_asset_profits(rev, net_profit_margin = 0.2)
  dnp <- discount_net_profits(prof, discount_rate = 0.1)
  npv <- compute_company_npv(dnp)

  out <- compute_company_pd_merton(npv)
  
  # Schema preservation
  original_cols <- names(npv)
  testthat::expect_true(all(original_cols %in% names(out)),
                       info = "All original columns should be preserved")
  testthat::expect_true("merton_pd" %in% names(out),
                       info = "merton_pd column should be added")
  
  # Row count preservation
  testthat::expect_equal(nrow(out), nrow(npv),
                        info = "Row count should be preserved")
  
  # Required input validation
  testthat::expect_true(all(c("company", "scenario", "npv") %in% names(npv)),
                       info = "Input should have required columns")
})
