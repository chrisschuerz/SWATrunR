
SWATplusR <img src="man/figures/swatr_hex.svg" align="right" />
===============================================================

The `SWATplusR` package provides tools to link existing SWAT2012 and SWAT+ models with your modeling workflows in R. `SWATplusR` enables you to execute SWAT simulations and to control all relevant parameters of a SWAT simulation, such as changes in model parameters, the simulation periods and time steps, or the simulated variables that should be returned to R. The central goal of `SWATplusR` is to return simulation results in a *tidy* format to facilitate an easy implementation of SWAT simulations, together with other R packages into clean and efficient R programming workflows. To efficiently handle large SWAT projects with large numbers of model evaluations and/or large simulation outputs, `SWATplusR` provides parallel computation and incremental saving and selective loading of simulation results into and from SQLite data bases.

Installation
------------

`SWATplusR` is currently under development. You can install an unreleased version of `SWATplusR` from the package's GitHub repository.

``` r
# If you do not have the package devtools installed
install.packages("devtools")

# If access is denied, then the repository might be still set to private
# In that case, please contact me (c.schuerz@posteo.org).
devtools::install_github("chrisschuerz/SWATplusR")
```

<!---You can install the released version of SWATplusR from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("SWATplusR")
```
--->
Workflow
--------

<img src="man/figures/package_workflow.svg" width="60%" style="display: block; margin: auto;" />

Getting started with `SWATplusR`
--------------------------------
