---
editor_options: 
  markdown: 
    wrap: 72
---

# breathtestcore: <sup>13</sup>C breath test to analyze gastric emptying

Dieter Menne Menne Biomed Consulting Tübingen, Germany
<http://www.menne-biomed.de>
[dieter.menne\@menne-biomed.de](mailto:dieter.menne@menne-biomed.de)

<!-- badges: start -->

[![R build
status](https://github.com/dmenne/breathtestcore/workflows/R-CMD-check/badge.svg)](https://github.com/dmenne/breathtestcore/actions)
![CRAN](https://cranlogs.r-pkg.org/badges/grand-total/breathtestcore)
![CRAN](https://www.r-pkg.org/badges/version-ago/breathtestcore)
[![Codecov test
coverage](https://codecov.io/gh/dmenne/breathtestcore/branch/master/graph/badge.svg)](https://codecov.io/gh/dmenne/breathtestcore?branch=master)

<!-- badges: end -->

Menne Biomed Consulting Tübingen, Germany <https://www.menne-biomed.de>

[dieter.menne\@menne-biomed.de](mailto:dieter.menne@menne-biomed.de)

-   [Online reference, examples with images and
    vignettes](https://dmenne.github.io/breathtestcore/).
-   To test some of the functions with sample data or your own data, try
    the [online demo](https://apps.menne-biomed.de/breathtestshiny).
-   Issues can be reported
    [here](https://github.com/dmenne/breathtestcore/issues).

This is a reboot of R package
[dmenne/d13cbreath](https://github.com/dmenne/d13cbreath) which is no
longer maintained.

## What it does

-   Reads several file formats of <sup>13</sup>C data: IRIS/Wagner
    (composite and CSV), BreathID and Excel.
-   Fits Beta-Exponential nonlinear curve fits using `nls`, which gives
    successful estimates for 90% of PDR curves.
-   Computes population fits with `nlme` when data from multiple
    recordings are available, resulting in much more reliable estimates
    for studies.
-   Computes prior-constrained Bayesian non-linear fit for single
    records (refactored to package
    [dmenne/breathteststan](https://github.com/dmenne/breathteststan))
-   Computes Bayesian non-linear population fit with Stan for multiple
    records (refactored to package dmenne/breathteststan)
-   Includes three data sets of <sup>13</sup>C records from the
    University Hospital of Zürich
-   [A comparison of results with nls,
    nlme](https://menne-biomed.de/blog/breath-test-stan) and Bayesian
    [Stan](http://www.mc-stan.org).
-   See the example in the documentation of `t50BluckCoward` for a
    comparison with published data.

## Sponsors and supporters

The software is being developed in cooperation with the Department of
Gastroenterology of the University Hospital of Zürich and Claraspital
Basel. Thanks to Benjamin Misselwitz, Mark Fox and Werner Schwizer.

## How to install

To install the most recent versions of the package, use

    devtools::install_github("dmenne/breathtestcore", build_vignettes = TRUE)
    # In case you want to use the fancy Stan-based methodes
    devtools::install_github("dmenne/breathteststan")
    # And here the web app; this is not on CRAN and must be installed from github
    devtools::install_github("dmenne/breathtestshiny", build_vignettes = TRUE)

Do not forget to use `build_vignettes = TRUE`.

Stable version of the packages
[breathtestcore](https://CRAN.R-project.org/package=breathtestcore) and
[breathteststan](https://CRAN.R-project.org/package=breathteststan) can
also be installed from CRAN.

For an easy installation, use the Docker image
[dmenne/breathtestshiny](https://hub.docker.com/r/dmenne/breathtestshiny)

You can run the [web app
online](https://apps.menne-biomed.de/breathtestshiny/). No data are
stored, but you can download all results and a series of tests for
studies.

## Usage example

This example is from the documentation of function
[nlme_fit](https://dmenne.github.io/breathtestcore/reference/nlme_fit.html).

    library(breathtestcore)
    d = simulate_breathtest_data(n_records = 3, noise = 0.2, seed = 4711)
    data = cleanup_data(d$data)
    fit = nlme_fit(data)
    plot(fit) # calls plot.breathtestfit

For additional examples, see the documentation and the tests in folder
`tests/testthat` of the source package.

## Planned

The core fitting functions and the Stan variants are reasonably stable
and can be used to analyze your breath test data with R. The
[Shiny](https://shiny.rstudio.com) web app with reporting is work in
progress; [online demo](https://apps.menne-biomed.de/breathtestshiny),
[source code](https://github.com/dmenne/breathtestshiny).

**Reference**:

-   Ghoos, Y. F., B. D. Maes, B. J. Geypens, G. Mys, M. I. Hiele, P. J.
    Rutgeerts, and G. Vantrappen. 1993. "Measurement of Gastric Emptying
    Rate of Solids by Means of a Carbon-Labeled Octanoic Acid Breath
    Test." *Gastroenterology* 104 (6). Department of Medicine,
    University Hospital Gasthuisberg, Belgium.: 1640–7.

-   Maes, B. D., B. J. Geypens, Y. F. Ghoos, M. I. Hiele, and P. J.
    Rutgeerts. 1998. "<sup>13</sup>C-Octanoic Acid Breath Test for
    Gastric Emptying Rate of Solids." *Gastroenterology* 114 (4):
    856–59.

-   Bluck LJC (2009) Recent advances in the interpretation of the
    <sup>13</sup>C octanoate breath test for gastric emptying. J. Breath
    Res. 3, <https://iopscience.iop.org/1752-7163/3/3/034002/>

-   Bluck, LJC, Jackson S, Vlasakakis G, Mander A (2011) Bayesian
    hierarchical methods to interpret the <sup>13</sup>C-octanoic acid
    breath test for gastric emptying. Digestion 83_96-107.
