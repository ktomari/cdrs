# read_example works

    Code
      cdrs_read_example()
    Message
      Loading fabricated DRS data. Do not draw conclusions from analyses of this synthesized data.
    Output
      # A tibble: 200 x 148
         DRS_ID  DistributionChannel UserLanguage geoid.county Zone  Q1_0  Q1_1  Q1_2 
         <fct>   <fct>               <fct>        <fct>        <fct> <fct> <fct> <fct>
       1 DRS2295 Online              English      013          3     <NA>  Yes   <NA> 
       2 DRS2296 Mail                English      013          3     Yes   No    No   
       3 DRS2297 Mail                Spanish      095          1     Yes   Yes   No   
       4 DRS2298 Online              English      013          2     No    Yes   <NA> 
       5 DRS2299 Online              Spanish      <NA>         3     Yes   Yes   No   
       6 DRS2300 Mail                Spanish      067          2     Yes   No    Yes  
       7 DRS2301 Online              Spanish      095          1     Yes   <NA>  Yes  
       8 DRS2302 Online              Spanish      095          2     Yes   Yes   Yes  
       9 DRS2303 Mail                Spanish      113          3     <NA>  <NA>  Yes  
      10 DRS2304 Online              English      067          3     Yes   No    No   
      # i 190 more rows
      # i 140 more variables: Q1_3 <fct>, Q1_4 <fct>, Q1_5 <fct>, Q1a <dbl>,
      #   Q2 <fct>, Q3_0 <fct>, Q3_1 <fct>, Q3_2 <fct>, Q3_3 <fct>, Q3_4 <fct>,
      #   Q3_5 <fct>, Q3_6 <fct>, Q3_7 <fct>, Q3_8 <fct>, Q4_0 <fct>, Q4_1 <fct>,
      #   Q4_2 <fct>, Q4_3 <fct>, Q4_4 <fct>, Q4_5 <fct>, Q6_0 <fct>, Q6_1 <fct>,
      #   Q6_2 <fct>, Q6_3 <fct>, Q6_4 <fct>, Q6_5 <fct>, Q6_6 <fct>, Q6_7 <fct>,
      #   Q6_8 <fct>, Q6_9 <fct>, Q7_0 <fct>, Q7_1 <fct>, Q7_2 <fct>, Q7_3 <fct>, ...

