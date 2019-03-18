
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
Functionality and workflow
--------------------------

The functionality of `SWATplusR` is reduced to a few essential functions that maintain the link between the SWAT project on the local hard drive and the R environment. With `load_demo()` you can retrieve demo data sets of SWAT projects, calibration data, and shape files of the demo catchment. With `run_swat2012()` and `run_swat2012()` you can run a SWAT model located in a local project folder and return simulation outputs to R that were defined with `define_output()`. Simulation results can be saved incrementally to an SQLite data base when a `save_file` is defined in `run_swat*()`. With `load_swat_run()` all or selected parts of the simulation results stored in the data base can be loaded back to R. `scan_swat_run()` scans the content of saved simulations and returns meta data on the saved content.

<img src="man/figures/package_workflow.svg" width="60%" style="display: block; margin: auto;" />

Getting started with `SWATplusR`
--------------------------------

You can explore the basic functionality of `SWATplusR` in the [Get started](https://chrisschuerz.github.io/SWATplusR/articles/SWATplusR.html#loading-demos) section. There you can learn the following basics:

-   Loading demo data [&gt;&gt;&gt;](https://chrisschuerz.github.io/SWATplusR/articles/SWATplusR.html#figures)
    -   SWAT projects [&gt;&gt;&gt;](https://chrisschuerz.github.io/SWATplusR/articles/SWATplusR.htmll#swat-projects)
    -   Observation data [&gt;&gt;&gt;](https://chrisschuerz.github.io/SWATplusR/articles/SWATplusR.html#observation-data)
    -   Spatial catchment data [&gt;&gt;&gt;](https://chrisschuerz.github.io/SWATplusR/articles/SWATplusR.html#spatial-data)
-   Performing first SWAT model runs from R [&gt;&gt;&gt;](https://chrisschuerz.github.io/SWATplusR/articles/SWATplusR.html#first-swat-model-runs)
    -   Output definition [&gt;&gt;&gt;](https://chrisschuerz.github.io/SWATplusR/articles/SWATplusR.html#output-definition)
    -   Exploring simulation results [&gt;&gt;&gt;](https://chrisschuerz.github.io/SWATplusR/articles/SWATplusR.html#spatial-data)
-   Defining parameter modifications for a SWAT run &gt;&gt;&gt;

-   Running SWAT with a single parameter set &gt;&gt;&gt;

-   Running SWAT with a table of parameter combinations &gt;&gt;&gt;

The [Articles](https://chrisschuerz.github.io/SWATplusR/articles/) section is a collection of tutorials for typical topics on SWAT modeling. Here you can learn how to use `SWATplusR` in combination with other R packages to perform tasks such as:

-   Parameter sensitivity analysis &gt;&gt;&gt;
-   Model parameter optimization &gt;&gt;&gt;
-   Parameter sampling and model calibration &gt;&gt;&gt;
-   Visualization &gt;&gt;&gt;

The Articles section will be updated in the future with further topics that can be relevant for any modeling workflow with `SWATplusR`.
