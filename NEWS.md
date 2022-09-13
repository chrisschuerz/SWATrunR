# SWATplusR 0.6.4

## Minor fix
- Fix issue with returning error message in `run_swatplus()`.

# SWATplusR 0.6.3

## Minor update
- Introduction of input argument `time_out` in `run_swatplus()`. Some SWAT+ parameter combinations lead to simulations to hang up. A workaround is to introduce a timeout for runs that get stuck. Now runs that time out return a time out error.

# SWATplusR 0.6.2

## Critical fix
- Writing parameter values to calibration.cal had issues with small values. Exponent was cut off then. This commit fixes the parameter writing to calibration.cal

# SWATplusR 0.6.1

## Minor fix
- Fix and improve message prompting for saving and screening simulations saved in databases.

# SWATplusR 0.6

## Major update 
- Saving and loading of SWAT simulations into SQLite data bases is fixed and works with new structure of simulation outputs
- Loading simulations with `load_swat_run()` is not downwards compatible, due to different structure of saved content.
- To load old SWAT runs that were saved to SQLite databases you have to downgrade `SWATplusR` to e.g. version 0.4

# SWATplusR 0.5.7
### Update
- Restructured output file reading in `run_swatplus()`:
  - Moved from `readr` to `data.table::fread` to increase speed and fix some minor issues
  - Update date vector reading to provide a safer adding of dates to outputs
  - Simplified internal structure of `define_output()` for SWAT+ outputs (Might be further restructured and extended in future).
  - get rid of `get_date_vector_plus()` as this step is now included in `read_swatplus_output()`
  - Update output reading in `run_swat2012()` to be compatible with the updated output reading.
  
# SWATplusR 0.5.6
### Fix
- Set lazy loading to default in all readr functions.

# SWATplusR 0.5.5
### Fix
- Fixes issue #58. Version 0.5.2 introduced a new way to read date vectors from SWAT+ simulations. There was a minor issue with multiple units when reading the date vector which is fixed now.


# SWATplusR 0.5.4
### Minor update
- Added auto irrigation parameters in .mgt inputs files of SWAT2012 projects to parameters that can be calibrated.

# SWATplusR 0.5.3
### Critical fix
The minor internal update of generating the date vector from SWAT+ simulation runs in version 0.5.2 caused now SWAT2012 runs to crash. This was pointed out in issue #55. This update fixes this issue.

# SWATplusR 0.5.2
### Major updates

- New input argument `start_date_print` was introduced in `run_swat_plus()`. This input overrules `years_skip` when both input arguments are provided. `start_print_date` offers greater flexibility for writing simulation outputs, as `years_skip` skips the number of years and then starts with Jan. 1. after the skipped years. With `start_date_print` now a date can be defined when to start printing. The end date for printing is still controlled by the input argument `end_date`.

- The pointer to `calibration.cal` that defines the parameter changes is now automatically written into the `file.cio`. This solves an old FAQ and frequent issue when no changes in the simulation outputs occur although parameter changes were defined.

### Minor internal updates

- The date vector for SWAT+ runs is now retrieved from the last simulation run. This fixes potential issues with outputs where not entire years were written and cases can occur where the generated date vector does not match the actual simulation outputs.

- A potential `soft_cal` option was removed from the code, as there is no future plan to include soft calibration directly into `SWATplusR`.


# SWATplusR 0.5.1
### Fixed issues

- Fixing issues running newer SWAT+ revisions (e.g. 60.5.4):
  - Fixing issues in SWAT+ parameter conditioning routine that was caused due to missing `reservoir.res` file.
  - Fixing issues with ouput files trailing NA values due to blanks in written SWAT+ output files.

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
