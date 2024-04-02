# To-Do List

This to-do list is not a wish list of desired features, but are rather issues that remain unresolved, but may have been patched with a temporary solution.

## Documentation

Update documentation about MAR and other missingness in the missing data doc.

## Function Features & Efficiency

* load_data > cdrs_composite_index.
    * This is too slow! Using base-R should improve speed.
    * Determine if we want "uncertainty" levels in index scores that are a `mean()`. Current assumption: no. This is an error for idx_olcc in original code for summary report.
* thematic plots don't work
* Q15's plot has broken labels

## Tests

### svy_wrappers

There is a known feature of dplyr::filter() that yields a data.frame that has adjusted rownames, such that they are reconstructed. For example, a data.frame with 4 rows, with rownames 1:4, once filtered to remove the second row, will now have rownames `(1,2,3)`, instead of `(1,3,4)`. A short term workaround has been created, but we should either assume all data.frames will reset their rownames, or simply convert everything to a tibble.
