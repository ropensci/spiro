spiro (development version)
===========================

  * Improved input validation
  * Rename add_weight() to add_bodymass() and rename weight argument in spiro()
  * Fix bug in spiro_plot() when raw data contained time duplicates


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
