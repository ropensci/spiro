spiro (development version)
===========================

* Fix bug in ZAN import


spiro 0.2.1 (2023-08-14)
===========================

### BUG FIXES

* Internal fix due to changes in R-devel
* Fix bug in printing spiro data frames when non-numeric columns were manually added


spiro 0.2.0 (2023-05-09)
===========================

### NEW FEATURES

* More customization options to spiro_plot(): Adjustable point and line sizes and colors with the style_args argument; optional display of vertical lines in spiro_plot() at the start of warm-up, first load and last load with the vert_lines argument
* New function spiro_raw() to access or import raw data only. This is a wrapper around attr(data, "raw") for spiro objects and replaces spiro_import() for raw data files, which has been deprecated as its name led to confusion that it might be a general import function.

### MINOR IMPROVEMENTS

* Smaller default point size in spiro_plot()

### BUG FIXES

* Fix bug in device detection when cortex files contained a header with an id field
* Fix bug in Cortex import when time data contained milliseconds
* Fix bug in import when raw time data contained invalid values (e.g. when the content of a row, but not the row itself was deleted in the excel raw data file)
* Hide secondary load axis in spiro_plot(which = 3), when no load data is available
* Improved Vyntus import for German language
* Fixed rare bug when the time data had an identical value present more than three times.

### DOCUMENTATION FIXES

* Updated citation information (A software paper for the spiro package was accepted by the Journal of Open Source Software)
* Updated documentation and vignette to reflect changes in spiro_plot().
* General documentation improvements
* Updated testing infrastructure (new soft dependency 'vdiffr' for snapshot testing of spiro_plot() output)


spiro 0.1.2 (2022-11-13)
===========================

### NEW FEATURES

* Add support for Vyntus files in French (thanks to Virgile Lecoultre)

### MINOR IMPROVEMENTS

* Add last.duration argument in pt_steps() and pt_const()
* Allow import of files with missing body mass data without setting a new body mass
* Improve detection of post exercise measurements during automated exercise protocol detection
* Improve plotting of none breath-by-breath data
* Improve NA handling in spiro_smooth() for zero-lag Butterworth filters
* Update spiro_plot() to recent changes in ggplot2 version 3.4.0

### BUG FIXES

* Fix bug in spiro_plot() when PET data was available and raw time data contained duplicates
* Fix bug in spiro_max() for RERmax calculation when data contained a long period without gas exchange measurements at the end

spiro 0.1.1 (2022-08-25)
===========================

First CRAN release.

### DOCUMENTATION FIXES

  * Fixes to prepare CRAN release


spiro 0.1.0 (2022-08-18)
===========================

First stable release. Reflects changes in response to the rOpenSci peer-review process. The repository has been transferred to the rOpenSci organization.

### MINOR IMPROVEMENTS

   * Rename helper functions for exercise protocol setting. pre(), wu(), steps(), const() are now termed pt_pre(), pt_wu(), pt_steps(), pt_const().
   * Rename get_id() to get_anonid()
   * Allow customization of plot arrangement in spiro_plot()

### DOCUMENTATION FIXES

  * Add more examples to function documentation


spiro 0.0.5 (2022-06-07)
===========================

### MINOR IMPROVEMENTS

  * Rename add_weight() to add_bodymass() and rename 'weight' argument in spiro() to 'bodymass'
  * Improved input validation

### BUG FIXES 

  * Fix bug in spiro_plot() when raw data contained time duplicates

### DOCUMENTATION FIXES

  * New contributing guidelines
  * New 'Background' section in README


spiro 0.0.4 (2022-05-15)
===========================

### MINOR IMPROVEMENTS

  * spiro_smooth() was rewritten and got new arguments. Digital filters in spiro_smooth() now work with the original raw data if possible (and not with the interpolated data).
  * spiro_max() output now also includes RERmax
  
### BUG FIXES
  
  * 'signal' is now a hard dependency
  * Fix bug in Cortex import assigning wrong units in some cases
  * Fix bug in Butterworth filter leading to errors in rare cases
  
### DOCUMENTATION FIXES

  * Improved README, vignettes and documentations

spiro 0.0.3 (2022-03-15)
===========================

### NEW FEATURES

  * The package now provides different filtering methods (time-based averages, breath-based averages, Butterworth filters) to smooth data in spiro_max() and spiro_plot()
  * Meta data is now anonymized during import. This can be disabled by using spiro(anonymize = FALSE)
  * Cortex import now works for files in English language (thanks to Sebastian Mühlenhoff)
  
### MINOR IMPROVEMENTS

  * Create a print method for spiro objects to print rounded data
  * Improved plotting
  * New dependency: digest, signal. Removed dependency: zoo
  * spiro_plot() now uses a zero-lag Butterworth filter as default data smoothing method
  
### BUG FIXES

  * Fix bug for Cosmed meta data import, reading age instead of birthday
  

spiro 0.0.2 (2022-01-03)
===========================

### NEW FEATURES

  * spiro() now supports the import of Cortex xml and Vyntus files (thanks to Daniel Appelhans, Sebastian Mühlenhoff, Adrian Swoboda & Andreas Wagner)
  * spiro_max() can now smooth over a defined number of breaths (and not only time frame)
  
### MINOR IMPROVEMENTS

  * Improved import for Cosmed files (thanks to Yannick Schwarz)
  * Improved protocol guessing
  * spiro() now checks if the imported raw data is breath-by-breath and displays a warning if this is not the case
  * spiro(file, protocol = NA) is now possible to avoid automated protocol guessing without specifying a protocol
  * add_protocol() and related function now support unusually short protocols and have an improved input validation
  * Output data frames of spiro_summary() now contain a 'load' column
  * Increased test coverage

  
### BUG FIXES
  * Fix bug in Cosmed import causing problems in visualization
  * Fix bug in ZAN import leading to incorrect protocol detection in rare 
  cases
  * Fix bug in set_protocol_manual() when input was a data frame
  * Fix bug in add_weight() that lead to calculation of RE for cycling protocols
  
### DOCUMENTATION FIXES
  * Improved README and CITATION file

spiro 0.0.1 (2021-11-24)
========================

### NEW FEATURES

  * First GitHub release
