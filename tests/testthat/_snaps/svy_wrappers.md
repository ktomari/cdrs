# svydesign w/ fpc works with example

    Code
      cdrs_design(data_ = demo, set_fpc = T)
    Output
      Stratified Independent Sampling design
      survey::svydesign(
            # This arg specifies cluster ids. In our case, the DRS has no clusters.
            ids = ~1,
            # Here we set the finite population correction, which *may* matter only
            # in cases where Zone 1 residents are of concern. See docs.
            fpc = ~N,
            # The cleaned data set, including the removal of missing values.
            data = data_,
            # Specify the strata (in our case Zone geographies)
            strata = ~Zone,
            # Specify the column with weights.
            weights = ~WTFINAL
          )

