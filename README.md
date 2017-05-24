[![Travis-CI Build Status](https://travis-ci.org/dmenne/breathtestcore.svg?branch=master)](https://travis-ci.org/dmenne/breathtestcore)
[![Coverage Status](https://coveralls.io/repos/github/dmenne/breathtestcore/badge.svg?branch=master)](https://coveralls.io/github/dmenne/breathtestcore?branch=master)
![CRAN](https://cranlogs.r-pkg.org/badges/last-week/breathtestcore)

breathtestcore: <sup>13</sup>C breath test to analyze gastric emptying
===========================================

Dieter Menne   
Menne Biomed Consulting Tübingen, Germany    
http://www.menne-biomed.de   

dieter.menne@menne-biomed.de 

* [Online reference, examples with images and vignettes](https://dmenne.github.io/breathtestcore/). 
* To test some of the functions with sample data or your own data, try the [online demo](https://apps.menne-biomed.de/breathtestshiny).
* Issues can be reported [here](https://github.com/dmenne/breathtestcore/issues).

This is a reboot of R package [dmenne/d13cbreath](https://github.com/dmenne/d13cbreath) which is partially obsolete. 

## What it does

* Reads several file formats of <sup>13</sup>C data: IRIS/Wagner (composite and CSV), BreathID and generic CSV
* Fits Beta-Exponential nonlinear curve fits using `nls`, which gives successful estimates for 90% of PDR curves
* Computes population fits with `nlme` when data from multiple recordings are available, resulting in much more reliable estimates for studies.
* Computes prior-constrained Bayesian non-linear fit for single records (refactored to package [dmenne/breathteststan](https://github.com/dmenne/breathteststan))
* Computes Bayesian non-linear population fit with Stan for multiple records (refactored to package dmenne/breathteststan)
* Includes an extensive data set of <sup>13</sup>C records from the University Hospital of Zürich  
* [A comparison of results with nls, nlme](http://menne-biomed.de/blog/breath-test-stan) and Bayesian [Stan](http://www.mc-stan.org).
* See the example in the documentation of `t50BluckCoward` for a comparison with published data. Most cases agree with those published here, but there are some exceptions?

## Sponsors and supporters

The software is being developed in cooperation with the Department of Gastroenterology of the University Hospital of Zürich and Claraspital Basel. Thanks to Benjamin Misselwitz, Mark Fox and Werner Schwizer. And special thanks to Andreas Steingötter for constantly reminding me that better statistics does necessarily make a method physiologically relevant.

## How to install
To install the functions, use

    devtools::install_github("breathtestcore","dmenne")
    # In case you want to use the fancy Stan-based methodes
    devtools::install_github("breathteststan","dmenne")
    # And here the still rudimentary web GUI
    devtools::install_github("breathtestshiny","dmenne")

## Usage example 
    
This example is from the documentation of function [nlme_fit](https://dmenne.github.io/breathtestcore/reference/nlme_fit.html).

    library(breathtestcore)    
    d = simulate_breathtest_data(n_records = 3, noise = 0.2, seed = 4711)
    data = cleanup_data(d$data)
    fit = nlme_fit(data)
    plot(fit) # calls plot.breathtestfit

For additional examples, see the folder `tests/testthat` of the source package.
    
## Previous and planned work
The core fitting functions and the Stan variants are quite stable and can be used to analyze your breath test data with R. For the Stan variants, additional models that give credible intervals for differences between groups are planned. The [Shiny](https://shiny.rstudio.com) web app with reporting is work in progress; [online demo](https://apps.menne-biomed.de/breathtestshiny), [source code](https://github.com/dmenne/breathtestshiny). 



__Reference__: 

* Ghoos, Y. F., B. D. Maes, B. J. Geypens, G. Mys, M. I. Hiele, P. J. Rutgeerts, and G. Vantrappen. 1993. “Measurement of Gastric Emptying Rate of Solids by Means of a Carbon-Labeled Octanoic Acid Breath Test.” *Gastroenterology* 104 (6). Department of Medicine, University Hospital Gasthuisberg, Belgium.: 1640–7.

* Maes, B. D., B. J. Geypens, Y. F. Ghoos, M. I. Hiele, and P. J. Rutgeerts. 1998. “<sup>13</sup>C-Octanoic Acid Breath Test for Gastric Emptying Rate of Solids.” *Gastroenterology* 114 (4): 856–59.

* Bluck LJC (2009) Recent advances in the interpretation of the <sup>13</sup>C octanoate breath test for gastric emptying. J. Breath Res. 3, http://iopscience.iop.org/1752-7163/3/3/034002/

* Bluck, LJC, Jackson S, Vlasakakis G, Mander A (2011) Bayesian hierarchical methods to interpret  the <sup>13</sup>C-octanoic acid breath  test for gastric emptying. Digestion 83_96-107.
