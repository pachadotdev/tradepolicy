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
