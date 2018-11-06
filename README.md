
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/cboettig/rdftools.svg?branch=master)](https://travis-ci.org/cboettig/rdftools)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/cboettig/rdftools?branch=master&svg=true)](https://ci.appveyor.com/project/cboettig/rdftools)
[![Coverage
status](https://codecov.io/gh/cboettig/rdftools/branch/master/graph/badge.svg)](https://codecov.io/github/cboettig/rdftools?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/rdftools)](https://cran.r-project.org/package=rdftools)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# rdftools

:package: Common utilities for RDF. Currently the package merely
provides a `write_nquads` S3 method for `data.frame` and `list` objects
to coerce them into the [N-Quads
format](https://en.wikipedia.org/wiki/N-Triples#N-Quads). Additional
utilities may be added at a later date.

## Installation

You can install the released version of rdftools from GitHub with:

``` r
remotes::install_github("cboettig/rdftools")
```

## Basic utilities

`rdftools` can write `data.frame` and `list` objects into the Resource
Description Format (RDF) as an `nquads` file. This can then be imported
into common RDF Databases. See
[virtuoso](https://github.com/cboettig/virtuoso) or
[rdflib](https://github.com/ropensci/rdflib) for examples importing
`nquads` files and working with RDF.

``` r
library(rdftools)
```

``` r
write_nquads(iris, "iris.nq", prefix = "iris")
```

``` r
x <- list(A1 = list(B = 1, C = 2), A2 = "bob")
write_nquads(x, "a_list.nq", prefix = "x")
```

## Experimental DSL

`rdftools` also hosts an experimental DSL for constructing SPARQL
queries.

-----

Please note that the `rdftools` project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.
