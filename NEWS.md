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
