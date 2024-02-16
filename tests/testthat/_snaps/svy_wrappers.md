# svydesign w/ fpc works with example

    Code
      serialized
    Output
          Q3_5 Zone   WTFINAL      N    probs
      68   Yes    2 0.9279586 540340 1.077634
      167   No    2 0.9430934 540340 1.060340
      129  Yes    3 0.9035757 166085 1.106714
      162   No    1 0.8974757  11727 1.114236
      43    No    3 0.2189963 166085 4.566287

# crosstab w/ works with example

    Code
      cdrs_crosstab(data_ = demo, cols_ = c("SEX_P", "Q3_5"))
    Output
        Q3_5 SEX_PFemale SEX_PMale se.SEX_PFemale se.SEX_PMale
      1   No    64.97260  31.05917      15.779190     9.560387
      2  Yes    29.26014  30.59424       8.187815     8.542056

