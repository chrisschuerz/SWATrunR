# SWATplusR 0.5

### Critical fix
This updated fixes issue #38 where simulations hang up on Windows. The reason is a major update for the `readr` package that introduced lazy reading with version 2.0. Lazy reading causes issues on Win systems and locks files that cannot be deleted and rewritten. This causes simulations to run dead. Please also update the `readr` package with this updated of `SWATplusR`. 


# SWATplusR 0.4

### Major updates:

- **Parameter conditions for SWAT+**:
  With this release a parameter conditioning is now possible for SWAT+ that is in its syntax similar to the one that was already possible for SWAT2012. This allows now to define parameter changes based on characteristics such as `unit`, `hsg`, `texture`, `landuse`, `plant`, or `slope`, among others.
- Reintroduction of the conditioning variable `layer` for SWAT2012 parameter conditioning. Fixes issue #32
- Move to `processx` instead of `system` for running executables. This fixes issues with running on Unix Systems
- Errors in model runs are now logged in the simulation outputs. This results in more stable simulations and provides information on failed model runs.

### Minor (internal) updates
- Updated parameter reading and writing for SWAT2012 parameters
- Update and fix parameter conditioning syntax translation for SWAT2012 


### Fixed issues:
- Fixed issue #42 
- Fixed issue with warning message `> Warning: Missing column names filled in: 'X6' [6]`
- Fixed issues with `case_when` in SWAT2012 parameter modification routine
- Fix issue #32
- Fix issue #23


# SWATplusR 0.3.2

### Major additions:

- Update routines to be compatible with R 4.0.x
- Major modification of outputs of `define_output()`. This was critical to speed up the output reading routine with a large number of output variables.
- Implement error logging of failed SWAT simulation runs.

### Minor additions:
- Add a start and end date check routine
- Implement input argument `revision` in `run_swatplus()` as workaround for failing revision check.
-minor updates in the function help files.

### Major issues:
- Fix wrong paths in the run SWAT batch files on Windows.
- Fix critical issue with modification of .mgt parameters in SWAT2012
- Fix critical issues with reading .hru outputs.

### Minor issues:
- Fix errors with revision checks in SWAT+
- Fix several minor issues with reading .mgt parameters
- Add .rsv parameters to be changeable in SWAT2012.
- Add .aqu parameters to be changeable in SWAT+
- Improve generalization of reading output file structures.


# SWATplusR 0.2.7

Tested release that includes all previous crucial fixes

# SWATplusR 0.2.7.9002

Development version that includes crucial fixes:
- Fixed issue with shifts in table headers that appear in SWAT+ rev59.3

# SWATplusR 0.2.7.9001

Development version that includes crucial fixes:
- Fixed issue with reading outputs from SWAT2012 rev670. 
- Fixed minor issue with table headers

# SWATplusR 0.2.6

Tested release that includes fixes from the development branch:
- Fixed issue with expressions in parameter constraints in SWAT2012
- Fixed issues with loading and saving in SWAT2012

# SWATplusR 0.2.6.9002

- Fixes issues with loading and saving in SWAT2012

# SWATplusR 0.2.6.9001

- Fixes issue with expressions in parameter constraints in SWAT2012

# SWATplusR 0.2.6.9000

- Fixed issue with `substr()` in R3.6.0
- Fixed issues with parameter constraining in `run_swat2012()`

# SWATplusR 0.2.5

- Included compatibility with SWAT+ revisions 55 and 59
- Added automatic revision check to `run_swatplus()`
- Transferred demo data sets to external package `SWATdata`
- Extended demo data to various SWAT+ revisions

# SWATplusR 0.2.4

This is the first tested [release](https://zenodo.org/record/2630510#.XKdiMtjgpGE) version of SWATplusR. The major functionality of running SWAT2012 and SWAT+ is available on win and unix platforms. Some functionality is still under development and/or depends on future developments of SWAT+. Functionality that is not available but will be implemented in future releases are:

-  A Linux SWAT+ demo data set
-  Constraining parameter changes for SWAT+


# SWATplusR 0.2.3.9002 

* Still development version in test phase
* Fixed issues with modification of \*.sub files for SWAT2012
* Fixed issues with modification of \*.sol files for SWAT2012
* Fixed issues with constraining of parameter modification for SWAT2012

# SWATplusR 0.2.3.9001 

* The package is still under development.
* Minor modifications in the help files.
* Minor bug fixes with not used dependencies.

# SWATplusR 0.2.3.9000 

* The package is still under development. An official release version of the package will be available soon.
