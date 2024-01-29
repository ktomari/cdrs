
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cdrs

<!-- badges: start -->
<!-- badges: end -->

The intent of this package is to aid data scientists in analyzing the
California Delta Residents Survey of 2023.

## Installation

You can install the development version of cdrs from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ktomari/cdrs")
```

## Example

First, we’ll need a sample from the data set.

``` r
data <- data.frame(
  DRS_ID = paste0("DRS", 2:6),
  Q2 = c("<I don't know>", rep("Historic or Delta \"legacy\" town", 4))
)
```

While this function is rarely called on its own, we’ll start by looking
at a component of the function that reads the data into the R
environment. This component allows the reading function to convert angle
bracketed values (eg. `"<Missing>"`) to `NA`.

``` r
library(tidyverse)
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.4     ✔ readr     2.1.5
#> ✔ forcats   1.0.0     ✔ stringr   1.5.1
#> ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
#> ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
#> ✔ purrr     1.0.2     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
library(cdrs)

cdrs_as_NA(data)
#>   DRS_ID                              Q2
#> 1   DRS2                            <NA>
#> 2   DRS3 Historic or Delta "legacy" town
#> 3   DRS4 Historic or Delta "legacy" town
#> 4   DRS5 Historic or Delta "legacy" town
#> 5   DRS6 Historic or Delta "legacy" town
```

## Example 2: Fabricated data.

The primary function to read in DRS data is `cdrs_read()`, however if
you don’t immediately have access to the public data set (or a
collaborator data set), you can still test it with this wrap-around
function `cdrs_read_example()`. This loads a fabricated data set that is
configured to look like the public data set circa December 2023. This
fabricated data set is in the package directory `/extdata/demo`.

``` r
dat1 <- cdrs::cdrs_read_example()
#> Loading fabricated DRS data.
head(dat1)
#> # A tibble: 6 × 148
#>   DRS_ID   DistributionChannel UserLanguage geoid.county Zone  Q1_0  Q1_1  Q1_2 
#>   <fct>    <fct>               <fct>        <fct>        <fct> <fct> <fct> <fct>
#> 1 DRS_ID2… Mail                English      113          1     Yes   No    Yes  
#> 2 DRS_ID2… Mail                English      095          1     Yes   No    Yes  
#> 3 DRS_ID2… Online              Spanish      113          2     Yes   <Dec… <Dec…
#> 4 DRS_ID2… Online              English      013          3     No    Yes   <Dec…
#> 5 DRS_ID2… Mail                Spanish      077          2     No    Yes   No   
#> 6 DRS_ID2… Mail                English      067          1     <Dec… No    No   
#> # ℹ 140 more variables: Q1_3 <fct>, Q1_4 <fct>, Q1_5 <fct>, Q1a <fct>,
#> #   Q2 <fct>, Q3_0 <fct>, Q3_1 <fct>, Q3_2 <fct>, Q3_3 <fct>, Q3_4 <fct>,
#> #   Q3_5 <fct>, Q3_6 <fct>, Q3_7 <fct>, Q3_8 <fct>, Q4_0 <fct>, Q4_1 <fct>,
#> #   Q4_2 <fct>, Q4_3 <fct>, Q4_4 <fct>, Q4_5 <fct>, Q6_0 <fct>, Q6_1 <fct>,
#> #   Q6_2 <fct>, Q6_3 <fct>, Q6_4 <fct>, Q6_5 <fct>, Q6_6 <fct>, Q6_7 <fct>,
#> #   Q6_8 <fct>, Q6_9 <fct>, Q7_0 <fct>, Q7_1 <fct>, Q7_2 <fct>, Q7_3 <fct>,
#> #   Q7_4 <fct>, Q7_5 <fct>, Q7_6 <fct>, Q7_7 <fct>, Q7_8 <fct>, Q7_9 <fct>, …
```

``` r
dat2 <- cdrs::cdrs_read_example(convert_to_NA = T)
#> Loading fabricated DRS data.
head(dat2)
#> # A tibble: 6 × 148
#>   DRS_ID   DistributionChannel UserLanguage geoid.county Zone  Q1_0  Q1_1  Q1_2 
#>   <fct>    <fct>               <fct>        <fct>        <fct> <fct> <fct> <fct>
#> 1 DRS_ID2… Mail                English      113          1     Yes   No    Yes  
#> 2 DRS_ID2… Mail                English      095          1     Yes   No    Yes  
#> 3 DRS_ID2… Online              Spanish      113          2     Yes   <NA>  <NA> 
#> 4 DRS_ID2… Online              English      013          3     No    Yes   <NA> 
#> 5 DRS_ID2… Mail                Spanish      077          2     No    Yes   No   
#> 6 DRS_ID2… Mail                English      067          1     <NA>  No    No   
#> # ℹ 140 more variables: Q1_3 <fct>, Q1_4 <fct>, Q1_5 <fct>, Q1a <dbl>,
#> #   Q2 <fct>, Q3_0 <fct>, Q3_1 <fct>, Q3_2 <fct>, Q3_3 <fct>, Q3_4 <fct>,
#> #   Q3_5 <fct>, Q3_6 <fct>, Q3_7 <fct>, Q3_8 <fct>, Q4_0 <fct>, Q4_1 <fct>,
#> #   Q4_2 <fct>, Q4_3 <fct>, Q4_4 <fct>, Q4_5 <fct>, Q6_0 <fct>, Q6_1 <fct>,
#> #   Q6_2 <fct>, Q6_3 <fct>, Q6_4 <fct>, Q6_5 <fct>, Q6_6 <fct>, Q6_7 <fct>,
#> #   Q6_8 <fct>, Q6_9 <fct>, Q7_0 <fct>, Q7_1 <fct>, Q7_2 <fct>, Q7_3 <fct>,
#> #   Q7_4 <fct>, Q7_5 <fct>, Q7_6 <fct>, Q7_7 <fct>, Q7_8 <fct>, Q7_9 <fct>, …
```

## Acknowledgements

I would like to thank the Dr. Jessica Rudnick, the Core R Team, the
authors of the Tidyverse, the authors of ‘R Packages’ 2nd Ed. (Wickham
and Bryan), Dirk Eddelbuettel et al. for the digest package, and last
but not least, Thomas Lumley for the survey package.

Initial funding was provided by Delta Science Program, Delta Stewardship
Council (DSC-21143).
