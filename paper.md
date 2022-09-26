---
title: 'spiro: An R package for analyzing data from cardiopulmonary exercise testing'
tags:
  - R
  - exercise science
authors:
  - name: Simon Nolte
    orcid: 0000-0003-1643-1860
    affiliation: 1
    corresponding: true
affiliations:
 - name: German Sport University Cologne, Institute of Movement and Neurosciences
   index: 1
date: 19 September 2022
bibliography: paper.bib
---

# Summary

Measuring gas exchange during physical exercise is a common procedure in sports science and medicine. It allows to assess the functional limit of the cardiovascular system, evaluate the success of training interventions, and diagnose cardio-respiratory diseases. The measuring devices of cardiopulmonary exercise testing --- so-called metabolic carts --- output their data in different formats. Moreover, measured breath-by-breath data is noisy and requires post-processing. The `spiro` package standardizes the import and processing of raw data from different metabolic carts.

# Statement of need

Data from cardiopulmonary exercise testing can be processed with different methods [@robergs2010]. Different processing strategies can influence key parameters calculated from the measurements, such as the maximum oxygen uptake [@martin-rincon2019]. This can in turn bias the evaluation of clinical conditions and intervention studies [@johnson1998; @martin-rincon2020]. The `spiro` package provides easy tools to compare and standardize processing methods for cardiopulmonary exercise testing.

In face of the 'replication crisis' in science, calls for more transparent research practices have reached the sports and exercise science community [@caldwell2020]. Transparent research in exercise science requires the sharing of analysis data and code [@borg2020]. In the sub-field of of exercise physiology, the `spiro` package now provides the open-source infrastructure to generate analysis code for cardiopulmonary exercise testing. When shared, this ultimately allows any researcher to reproduce study results.

The `spiro` package allows to process large amounts of data sets in considerably short time. Having accumulated large data sets from cardiopulmonary exercise testing, researchers can identify physiological patterns using techniques from machine or deep learning [@zignoli2019]. Together with packages for functional programming (e.g. `purrr` [@henry2020]) the `spiro` package can pre-process data before applying such advanced analysis algorithms.

# Acknowledgements

Thanks...

# References
