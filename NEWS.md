# tradepolicu 0.6.0

Breaking changes
* Uses fixest estimation as default
* For all the summary functions the default is "ppml" (it was "lm", now renamed to "ols")
* The summary functions were renamed to tp_summary_app_T instead of tp_summary_appT (T={1,2,3})

Removals
* tp_clustered_glm(), which doesn't make sense anymore after using fixest,
  which provides clustered standard errors directly

# tradepolicy 0.5.0

* Uses a single dataset + dplyr::filter for all the applications in the book
* All the other datasets (including solutions) were moved to GitHub's releases section

# tradepolicy 0.4.1

* Uses RDS instead of embedded SQL to accomplish all new CRAN rules

# tradepolicy 0.4

* This package is the continuation of yotover
* The tp_fixed_effects() function was re-written to allow more flexibility when extracting fixed effects
* Allows creating the database in non-default directories
* Less exported functions to avoid user distraction, focusing on usage over total flexibility
