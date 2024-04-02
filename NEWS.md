# cdrs 0.2.0

# cdrs 0.1.1

In an effort to reach our next minor update, we're trying to stabilize and make evergreen the parameters for existing functions, adding more robust tests, and adding {survey} function wrappers. Strictly speaking, we have indeed added new functionality, but the "feature" we're building up to is not complete (ie. replicating the Summary Report), so this is why it is a (big) patch and not a minor update.

## Evergreen Arguments

We changed functions that took `...` arguments since it was clear that it would be difficult or impossible to specify a character vector. This would have forced users to use NSE just to run batch operations on functions like `cdrs_subset`. Now, users are expected to submit a character vector. 

Importantly, we also changed the way to read in the DRS data while also specifying how to handle missingness values. Before, we just specified `convert_to_NA`, but then we realized that we may want to selectively choose which missingness values should be converted to `NA`. We've divided up missingness values into four categories (see our Docs), so that users can specify if they want certain missingness values like `<Decline to answer>`. This radically changed several functions in load_data.R. Practically speaking, if you run `cdrs_read("path/to/data")`, not much is different except that now, by default, all missingness values (other than those expressing uncertainty) are converted to `NA`. 

## Testing

We had to update how `survey.design` objects were stored as snapshots using {testthat} because it wasn't serializing in expected ways. A custom serialization function was added.

## Survey wrappers

The intent here is to replicate some of the functions used to create the 2023 Summary Report. These functions are meant to demonstrate how to correctly define parameters for functions from the {survey} package. Otherwise, they aren't really special (ie. they don't fundamentally do anything you couldn't do with {survey} functions before). These wrappers serve both as teaching tools (ie. what bespoke parameters are appropriate for the DRS data set) and hopefully reduce user error by automating much of the complex analytical survey specifications. The next "minor" update will be registered once these wrapper functions are completed.

# cdrs 0.1.0

Addition of `cdrs_subset` and `cdrs_design` which subset data according to requirements of `cdrs_design` and create a survey design object, respectively.

# cdrs 0.0.0.9000

Initial commit with `cdrs_read` and helpers.
