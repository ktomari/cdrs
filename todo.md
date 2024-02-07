# To-Do List

This to-do list is not a wish list of desired features, but are rather issues that remain unresolved, but may have been patched with a temporary solution.

## Documentation

For the function `cdrs_as_NA` in load_data.R, I have a decision tree that needs to be presented in the documentation more clearly. Below I create a basic md table demonstrating decisions for non-design variables. Design variables include `WTFINAL` and `Zone`.

| R Value | convert_as_NA = F | convert_as_NA = T |
| ------- | ----------------- | ----------------- |
| `NA` | As is. | As is. |
| `<Decline to answer>` | As is, except brackets are dropped. | All `NA`. We convert these values to `NA` because respondents explicitly skipped the question they read. This implies a withholding of information that we treat as missing data. |
| `<Not Applicable>` | For questions following `Q39` these values are converted to `NA` because respondents never read the question. For other questions, the angle brackets are dropped. | All of these values are converted to `NA`, otherwise functions like `svymean` would yield an error on a numeric variable. |
| `<I don't know>` | As is, except brackets are dropped. | Although this provides relevant information on questions, indicating a respondent's uncertainty, if we are only interested in clear affirmative or negative response, we can safely mark this as `NA`. Given this complexity, a warning message will trigger, unless otherwise turned off. |
| `<Missing>` |  As is, except brackets ar dropped. | `NA` |
| `<Erased>` |  As is, except brackets ar dropped. | `NA` |

## Tests

### svy_wrappers

There is a known feature of dplyr::filter() that yields a data.frame that has adjusted rownames, such that they are reconstructed. For example, a data.frame with 4 rows, with rownames 1:4, once filtered to remove the second row, will now have rownames `(1,2,3)`, instead of `(1,3,4)`. A short term workaround has been created, but we should either assume all data.frames will reset their rownames, or simply convert everything to a tibble.
