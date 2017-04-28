[![Travis-CI Build Status](https://travis-ci.org/dmenne/breathtestcore.svg?branch=master)](https://travis-ci.org/dmenne/breathtestcore)
[![Coverage Status](https://coveralls.io/repos/github/dmenne/breathtestcore/badge.svg?branch=master)](https://coveralls.io/github/dmenne/breathtestcore?branch=master)

breathtestcore: Analyze 13C time series from breath tests
===========================================

Dieter Menne   
Menne Biomed Consulting Tübingen, Germany    
http://www.menne-biomed.de   

dieter.menne@menne-biomed.de 

This is a reboot of R package [dmenne/d13cbreath](https://github.com/dmenne/d13cbreath) with better separation of functions. To test some of the functions with sample data or your own data, try the [online demo](https://apps.menne-biomed.de/breathtestshiny). Issues can be reported [here](https://github.com/dmenne/breathtestcore/issues).

## What it does

* Reads several file formats of 13C data: IRIS/Wagner (composite and CSV), BreathID and generic CSV
* Fits Beta-Exponential nonlinear function using `nls`, which gives successful estimates for 90% of PDR curves
* Computes population fit with `nlme` using all data in database
* Computes Bayesian non-linear population fit with Stan for multiple records (refactored to package dmenne/breathteststan)
* Computes prior-constrained Bayesian non-linear fit for single records (refactored to package [dmenne/breathteststan](https://github.com/dmenne/breathteststan))
* Includes an extensive data set of 13C records from the University Hospital of Zürich  
* [A comparison of results with nls, nlme](http://menne-biomed.de/blog/de/breath-test-stan) and Bayesian [Stan](http://www.mc-stan.org).
* See the example in the documentation of `t50BluckCoward` for a comparison with published data. Most cases agree with those published here, but there are some exceptions?

## Sponsors and supporters

The software is being developed in cooperation with the ETH, the Department of 
Gastroenterology of the University Hospital of Zürich, and Claraspital Basel. Thanks to Andreas Steingötter, Benjamin Misselwitz, Mark Fox and Werner Schwizer.


## How to install
To install the functions, use

    devtools::install_github("breathtestcore","dmenne")
    # In case you want to use the fancy Stan-based methodes
    devtools::install_github("breathteststan","dmenne")
    # And here the still rudimentary web GUI
    devtools::install_github("breathtestshiny","dmenne")

## Usage example 
    
This example is from the documentation of function `nlme_fit`.

    library(breathtestcore)    
    d = simulate_breathtest_data(n_records = 3, noise = 0.2, seed = 4711)
    data = cleanup_data(d$data)
    fit = nlme_fit(data)
    plot(fit) # calls plot.breathtestfit

For additional examples, see the folder `tests/testthat` of the source package.
    
## Previous and planned work
The core fitting functions and the Stan variants are quite stable and can be used to analyze your breath test data with R. For the Stan variants, additional models that give credible intervals for differences between groups are planned. The [Shiny-based web app](https://shiny.rstudio.com) with reporting, [online demo here](https://apps.menne-biomed.de/breathtestshiny), is work in progress. 



__Reference__: 

* Bluck LJC and Coward WA (2006) Measurement of gastric emptying by the C-13-octanoate breath test --- rationalization with scintigraphy. Physiol. Meas. 27 279-89

* Bluck LJC (2009) Recent advances in the interpretation of the 13C octanoate breath test for gastric emptying. J. Breath Res. 3, http://iopscience.iop.org/1752-7163/3/3/034002/

* Bluck, LJC, Jackson S, Vlasakakis G, Mander A (2011) Bayesian hierarchical methods to interpret  the 13C-octanoic acid breath  test for gastric emptying. Digestion 83_96-107.
