
<!-- README.md is generated from README.Rmd. Please edit that file -->

## `collin`

Collinearity can be a problem in regression models. When examining the
effects of an exposure at different time points, constrained distributed
lag models
(<a href="https://CRAN.R-project.org/package=dlnm" target="_blank">https://CRAN.R-project.org/package=dlnm</a>)
can alleviate some of the problems caused by collinearity. Still, some
consequences of collinearity may remain and they are often unexplored.
This package is a tool to assess whether unexpected results of a study
could be influenced by collinearity. Essentially, the package provides a
graphical comparison of the effects estimated in the real analysis with
the effects estimates that would be obtained in a scenario with an
alternative true pattern effect for the association of interest. The
package can be also applied to regression models that do not include a
distributed lag structure.

### Getting started

- The last version released on CRAN can be installed within an R session
  by executing:

``` r
install.packages("collin")
```

- The package collin is available on the Comprehensive R Archive Network
  (CRAN), with info at the related web page
  <a href="https://CRAN.R-project.org/package=collin" target="_blank">https://CRAN.R-project.org/package=collin</a>.

- Once the package has been installed, a summary of the main functions
  is available by executing:

``` r
help(collin)
```

- A comprehensive tutorial, including a number of detailed examples, is
  available by executing:

``` r
vignette("collin")
```

### References

The methodology used in the package is described in

- Basagaña X, Barrera-Gómez J. *Reflection on modern methods:
  visualizing the effects of collinearity in distributed lag models*.
  International Journal of Epidemiology. 2021;51(1):334-344. DOI:
  10.1093/ije/dyab179. URL:
  <https://academic.oup.com/ije/article/51/1/334/6359467>
