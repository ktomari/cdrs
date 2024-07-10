# cdrs_design. w/ fpc works with example

    Code
      serialized
    Output
          Q3_5 Zone   WTFINAL      N    probs
      68   Yes    2 0.9279586 540340 1.077634
      167   No    2 0.9430934 540340 1.060340
      129  Yes    3 0.9035757 166085 1.106714
      162   No    1 0.8974757  11727 1.114236
      43    No    3 0.2189963 166085 4.566287

# cdrs_crosstab. w/ works with example

    Code
      cdrs_crosstab(data_ = demo, cols_ = c("SEX_P", "Q3_5"), set_fpc = T)
    Output
              Q3_5
      SEX_P           No       Yes
        Female 0.4167952 0.1877020
        Male   0.1992426 0.1962602

# cdrs_props, return_stat = F, Q2

    Code
      obj
    Output
      # A tibble: 5 x 6
        variable levels                               mean     SE percent percent_lab
        <chr>    <fct>                               <dbl>  <dbl>   <dbl> <chr>      
      1 Q2       "Urban"                             0.145 0.0432      15 15%        
      2 Q2       "Suburban"                          0.181 0.0524      18 18%        
      3 Q2       "Historic or Delta \"legacy\" town" 0.229 0.0608      23 23%        
      4 Q2       "Rural (outside of town)"           0.218 0.0539      22 22%        
      5 Q2       "<I don't know>"                    0.227 0.0545      23 23%        

