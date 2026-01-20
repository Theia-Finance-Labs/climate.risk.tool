# compute_risk output matches snapshot

    Code
      results$companies
    Output
      # A tibble: 6 x 9
        company   NPV_baseline PD_baseline Expected_loss_baseline   NPV_shock PD_shock
        <chr>            <dbl>       <dbl>                  <dbl>       <dbl>    <dbl>
      1 Company 3    45683880.      0.0349                  1318.   45651739.   0.0350
      2 Company 4   109950565.      0.0123                  4561.  109852111.   0.0124
      3 Eneva SA   2359936579.      0.155                 618698. 2356917759.   0.155 
      4 AGRItest   1315656559.      0.0795                318191. 1315656559.   0.0795
      5 AGRItest2  1315656559.      0.0795                318191. 1315656559.   0.0795
      6 Alupar     1315656559.      0.0795                318191. 1313973577.   0.0797
      # i 3 more variables: Expected_loss_shock <dbl>, NPV_change_pct <dbl>,
      #   Expected_loss_change_pct <dbl>

