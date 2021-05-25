# tradepolicy: Replication of 'An Advanced Guide To Trade Policy Analysis'

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![GH-actions Windows](https://github.com/pachamaltese/tradepolicy/workflows/check-windows/badge.svg)](https://github.com/pachamaltese/tradepolicy/actions)
[![GH-actions Linux](https://github.com/pachamaltese/tradepolicy/workflows/check-linux/badge.svg)](https://github.com/pachamaltese/tradepolicy/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/tradepolicy)](https://cran.r-project.org/package=tradepolicy)
[![codecov](https://codecov.io/gh/pachamaltese/tradepolicy/branch/main/graph/badge.svg?token=9UOFPQHTF2)](https://codecov.io/gh/pachamaltese/tradepolicy)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4277741.svg)](https://doi.org/10.5281/zenodo.4277741)

# About

This R package, documented in a very similar way to the book [R4DS](https://r4ds.had.co.nz/), provides functions to replicate the original Stata results from the book [An Advanced Guide to Trade Policy Analysis](https://vi.unctad.org/tpa/web/Advanced%20Guide%20to%20TPA/readme.pdf).

For the analysis, this provides functions to compute robust clustered standard errors in generalized linear models. See https://r.tiid.org/R_structural_gravity/ for a fully detailed replication of AGTPA's results.

# Installation

## From CRAN

```
install.packages("tradepolicy")
```

## From GitHub

```
# install.packages("remotes")
remotes::install_github("pachamaltese/tradepolicy")
```

## Cite this work

If you use `tradepolicy` in academic works or other publication, please cite as follows:

```
Mauricio Vargas (2020). tradepolicy: An Advanced Guide to Trade Policy Analysis. R
  package version 0.2. https://pacha.dev/tradepolicy/
```

BibTeX entry:

```
@Manual{,
  title = {tradepolicy: Replication of 'An Advanced Guide To Trade Policy Analysis'},
  author = {Mauricio Vargas},
  year = {2021},
  note = {R package version 0.4.1},
  url = {https://pacha.dev/tradepolicy/},
  doi = {10.5281/zenodo.4277741}
}
```
