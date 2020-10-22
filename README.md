# yotover

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![GH-actions Windows](https://github.com/pachamaltese/yotover/workflows/check-windows/badge.svg)](https://github.com/pachamaltese/yotover/actions)
[![GH-actions Linux](https://github.com/pachamaltese/yotover/workflows/check-linux/badge.svg)](https://github.com/pachamaltese/yotover/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/yotover)](https://cran.r-project.org/package=yotover)
[![Coverage status](https://codecov.io/gh/pachamaltese/yotover/branch/master/graph/badge.svg)](https://codecov.io/github/pachamaltese/yotover?branch=master)

This R package, documented in a very similar way to the book [R4DS](), provides functions to replicate the origina Stata results from the book [An Advanced Guide to Trade Policy Analysis](https://vi.unctad.org/tpa/web/Advanced%20Guide%20to%20TPA/readme.pdf). 

Instead of providing large datasets in RDS or CSV format, this provides functions to create and use a [DuckDB]() SQL OLAP database with the aim of organizing the information in the best possible way, this approach is very similar to the observed in the [citesdb](https://github.com/ropensci/citesdb) package.

For the analysis, this provides functions to compute robust clustered standard errors in generalized linear models.

See https://pacha.dev/yotover for a fully detailed replication of Yotov's results.

At the moment, this package can be installed from GitHub:
```
# install.packages("remotes")
remotes::install_github("pachamaltese/yotover")
```
