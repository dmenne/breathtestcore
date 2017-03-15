[![Travis-CI Build Status](https://travis-ci.org/dmenne/breathtestcore.svg?branch=master)](https://travis-ci.org/dmenne/breathtestcore)
[![Coverage Status](https://coveralls.io/repos/github/dmenne/breathtestcore/badge.svg?branch=master)](https://coveralls.io/github/dmenne/breathtestcore?branch=master)

Package breathtestcore: Fitting C13 breath test data for gastric emptying studies
===========================================

Dieter Menne   
Menne Biomed Consulting Tübingen, Germany    
http://www.menne-biomed.de   

dieter.menne@menne-biomed.de 

## This package is under development. 

Use dmenne/d13breath instead until finished.

## What it does
The software is being developed in cooperation with the ETH and Department of Gastroenterology of the University Hospital of Zürich, Switzerland. Thanks to Andreas Steingötter, Benjamin Misselwitz, Mark Fox and Werner Schwizer.

* Reads several formats of 13C data: IRIS/Wagner, BreathID and generic CSV
* Fits Beta-Exponential nonlinear function using `nls`, which gives successful estimates for 90% of PDR curves
* Fits Wagner-Nelson, with terminal slope estimated from Bluck-Coward fit
* Computes population fit with `nlme` using all data in database
* Computes Bayesian non-linear population fit with Stan for multiple records
* Computes prior-constraing Bayesian non-linear fit for single records
* For additional examples, see the folder `tests/testthat` of the source package
* [A comparison of results with nls, nlme](http://menne-biomed.de/blog/de/breath-test-stan) and Bayesian [Stan](http://www.mc-stan.org).
* See the example in the documentation of `t50BluckCoward` for a comparison with published data. Most cases agree with those published here, but there are some exceptions; possible a typo in the published table?

## How to install
To install the functions, use
```
devtools::install_github("breathtestcore","dmenne")
```

## Previous and future work
This is a refactored version of github package `dmenne/d13cbreath` without database and display functionality. Use this package if you want to write your own processing with R. 
A Shiny-based interface with a patient database for installation on your computer system will be added later.

__Reference__: 

* Bluck LJC and Coward WA (2006) Measurement of gastric emptying by the C-13-octanoate breath test --- rationalization with scintigraphy. Physiol. Meas. 27 279-89

* Bluck LJC (2009) Recent advances in the interpretation of the 13C octanoate breath test for gastric emptying. J. Breath Res. 3, http://iopscience.iop.org/1752-7163/3/3/034002/

* Bluck, LJC, Jackson S, Vlasakakis G, Mander A (2011) Bayesian hierarchical methods to interpret  the 13C-octanoic acid breath  test for gastric emptying. Digestion 83_96-107.
