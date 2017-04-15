[![Travis-CI Build Status](https://travis-ci.org/dmenne/breathtestcore.svg?branch=master)](https://travis-ci.org/dmenne/breathtestcore)
[![Coverage Status](https://coveralls.io/repos/github/dmenne/breathtestcore/badge.svg?branch=master)](https://coveralls.io/github/dmenne/breathtestcore?branch=master)

breathtestcore: functions to read and fit 13C time series from breath tests
===========================================

Dieter Menne   
Menne Biomed Consulting Tübingen, Germany    
http://www.menne-biomed.de   

dieter.menne@menne-biomed.de 

## This package is under development. 

This is a reboot of package dmenne/d13cbreath with better separation of functions. Use dmenne/d13cbreath meanwhile.

## What it does
The software is being developed in cooperation with the ETH and Department of Gastroenterology of the University Hospital of Zürich, Switzerland. Thanks to Andreas Steingötter, Benjamin Misselwitz, Mark Fox and Werner Schwizer.

* Reads several formats of 13C data: IRIS/Wagner (composite and CSV), BreathID and generic CSV
* Fits Beta-Exponential nonlinear function using `nls`, which gives successful estimates for 90% of PDR curves
* Fits Wagner-Nelson, with terminal slope estimated from Bluck-Coward fit
* Computes population fit with `nlme` using all data in database
* Computes Bayesian non-linear population fit with Stan for multiple records (refactored to package dmenne/breathteststan)
* Computes prior-constrained Bayesian non-linear fit for single records (refactored to package dmenne/breathteststan)
* For additional examples, see the folder `tests/testthat` of the source package
* [A comparison of results with nls, nlme](http://menne-biomed.de/blog/de/breath-test-stan) and Bayesian [Stan](http://www.mc-stan.org).
* See the example in the documentation of `t50BluckCoward` for a comparison with published data. Most cases agree with those published here, but there are some exceptions?

## How to install
To install the functions, use
```
devtools::install_github("breathtestcore","dmenne")
# In case you want to use the fancey Stan-based methodes
devtools::install_github("breathteststan","dmenne")
# And here the currently quite rudimentary Shiny Gui
# No docs yet, waiting for a sponsor...
devtools::install_github("breathtestshiny","dmenne")
```

## Previous and future work
This is a refactored version of github package `dmenne/d13cbreath` without database and display functionality. Use this package if you want to write your own breath test processing with R.  A Shiny-based interface with a patient database for installation on your computer system will be added later (pending sponsoring)

__Reference__: 

* Bluck LJC and Coward WA (2006) Measurement of gastric emptying by the C-13-octanoate breath test --- rationalization with scintigraphy. Physiol. Meas. 27 279-89

* Bluck LJC (2009) Recent advances in the interpretation of the 13C octanoate breath test for gastric emptying. J. Breath Res. 3, http://iopscience.iop.org/1752-7163/3/3/034002/

* Bluck, LJC, Jackson S, Vlasakakis G, Mander A (2011) Bayesian hierarchical methods to interpret  the 13C-octanoic acid breath  test for gastric emptying. Digestion 83_96-107.
