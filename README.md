[![Travis-CI Build Status](https://travis-ci.org/dmenne/breathtestcore.svg?branch=master)](https://travis-ci.org/dmenne/breathtestcore)
[![Coverage Status](https://coveralls.io/repos/github/dmenne/breathtestcore/badge.svg?branch=master)](https://coveralls.io/github/dmenne/breathtestcore?branch=master)

breathtestcore: Read and fit 13C time series from breath tests
===========================================

Dieter Menne   
Menne Biomed Consulting Tübingen, Germany    
http://www.menne-biomed.de   

dieter.menne@menne-biomed.de 

## This package is under development. 

This is a reboot of package dmenne/d13cbreath with better separation of functions. To test some of the functions with sample data or your own data, try the [online demo](https://apps.menne-biomed.de/breathtestshiny).

## What it does
The software is being developed in cooperation with the ETH and Department of Gastroenterology of the University Hospital of Zürich, Switzerland. Thanks to Andreas Steingötter, Benjamin Misselwitz, Mark Fox and Werner Schwizer.

* Reads several formats of 13C data: IRIS/Wagner (composite and CSV), BreathID and generic CSV
* Fits Beta-Exponential nonlinear function using `nls`, which gives successful estimates for 90% of PDR curves
* Computes population fit with `nlme` using all data in database
* Computes Bayesian non-linear population fit with Stan for multiple records (refactored to package dmenne/breathteststan)
* Computes prior-constrained Bayesian non-linear fit for single records (refactored to package dmenne/breathteststan)
* For additional examples, see the folder `tests/testthat` of the source package
* [A comparison of results with nls, nlme](http://menne-biomed.de/blog/de/breath-test-stan) and Bayesian [Stan](http://www.mc-stan.org).
* See the example in the documentation of `t50BluckCoward` for a comparison with published data. Most cases agree with those published here, but there are some exceptions?

## How to install
To install the functions, use

    devtools::install_github("breathtestcore","dmenne")
    # In case you want to use the fancy Stan-based methodes
    devtools::install_github("breathteststan","dmenne")
    # And here the currently quite rudimentary Shiny Gui
    # No docs yet, waiting for a sponsor...
    devtools::install_github("breathtestshiny","dmenne")

## Usage example from function nlme_fit
    
    library(breathtestcore)    
    d = simulate_breathtest_data(n_records = 3, noise = 0.2, seed = 4711)
    data = cleanup_data(d$data)
    fit = nlme_fit(data)
    plot(fit) # calls plot.breathtestfit
    
## Previous and (maybe) future work
The core fitting functions and the Stan variant are quite stable and can be used to analyze your breath test data with R. The Shiny-based gui (github `dmenne/breathtestshiny` and [online demo here](https://apps.menne-biomed.de/breathtestshiny)) currently is quite rudimentary. Waiting for a sponsor to do further work..,



__Reference__: 

* Bluck LJC and Coward WA (2006) Measurement of gastric emptying by the C-13-octanoate breath test --- rationalization with scintigraphy. Physiol. Meas. 27 279-89

* Bluck LJC (2009) Recent advances in the interpretation of the 13C octanoate breath test for gastric emptying. J. Breath Res. 3, http://iopscience.iop.org/1752-7163/3/3/034002/

* Bluck, LJC, Jackson S, Vlasakakis G, Mander A (2011) Bayesian hierarchical methods to interpret  the 13C-octanoic acid breath  test for gastric emptying. Digestion 83_96-107.
