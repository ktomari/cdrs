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
        Q3_5 SEX_PFemale SEX_PMale se.SEX_PFemale se.SEX_PMale
      1   No    64.97260  31.05917      15.779190     9.560387
      2  Yes    29.26014  30.59424       8.187815     8.542056

# cdrs_props, return_stat = F, Q2

    Code
      obj
    Output
      # A tibble: 5 x 6
        variable levels                               mean     SE percent percent_lab
        <chr>    <fct>                               <dbl>  <dbl>   <dbl> <chr>      
      1 Q2       "Urban"                             0.145 0.0432      15 15%        
      2 Q2       "Suburban"                          0.181 0.0525      18 18%        
      3 Q2       "Historic or Delta \"legacy\" town" 0.229 0.0609      23 23%        
      4 Q2       "Rural (outside of town)"           0.218 0.0540      22 22%        
      5 Q2       "<I don't know>"                    0.227 0.0546      23 23%        

