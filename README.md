# yotover

This R package, documented in a very similar way to the book [R4DS](), provides functions to replicate the origina Stata results from the book [An Advanced Guide to Trade Policy Analysis](https://vi.unctad.org/tpa/web/Advanced%20Guide%20to%20TPA/readme.pdf). 

Instead of providing large datasets in RDS or CSV format, this provides functions to create and use a [DuckDB]() SQL OLAP database with the aim of organizing the information in the best possible way, this approach is very similar to the observed in the [citesdb](https://github.com/ropensci/citesdb) package.

For the analysis, this provides functions to compute robust clustered standard errors in generalized linear models.

See https://pacha.dev/yotover for a fully detailed replication of Yotov's results.
