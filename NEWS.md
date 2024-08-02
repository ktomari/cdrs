# cdrs 0.4.5

# cdrs 0.4.4

# cdrs 0.4.3

# cdrs 0.4.2

# cdrs 0.4.1

Corrected some bugs in gt.

# cdrs 0.4.0

This is a minor update for two reasons: `cdrs_crosstab` has been renamed and now breaks. And now crosstabulation has a corresponding image created using the \{gt\} package

# cdrs 0.3.5

Fixed bug in `plt_legend_nrow()`.

# cdrs 0.3.4

Adjusted legend labeling using a lm.

# cdrs 0.3.3

Added new conditional warning function `warning_c`.

# cdrs 0.3.2

# cdrs 0.3.1

# cdrs 0.3.0

There are considerable changes since 0.2.1, but much of it is under the hood. Here are some of the important changes in key features of the cdrs package:

1. Numeric variables are now supported in `cdrs_plt_prep` and concomitantly, you may now create histograms.
2. Major bugs with `cdrs_plt_prep`, especially for ordinal variables, have been fixed.
3. Improved text size adjustments using `title_size`.
4. Captions are now accessible as a named list, making manipulation of captions easier. (These adjustments should follow `cdrs_plt_prep`.) This ultimately facilitates an easier presentation of captions in \{shinycdrs\}.

Major organizational changes:

1. As `cdrs_plt_prep` became large and unwieldy, much of the code that tweaks the `props` (proportions tables derived from `survey::svymean`) has been transfered to utility/helper function `enrich_props`, which additionally has a number of its own helper functions.

Proposed future changes:

1. More complex plots that integrate an additional factor variable (eg. responses to Q1 by Zone).
2. A conceptual shift in how crosstabs are displayed. We expect to see row and column sums differently than how they are presented at this moment. (Essentially using `addmargins()` to the output of `svytable()`.) Expect to see this change in an upcoming patch (0.3.X). 

# cdrs 0.2.1

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
