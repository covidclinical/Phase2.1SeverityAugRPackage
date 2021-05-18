# Phase2.1SeverityAugRPackage
R code to run, validate, and submit the analysis for the SeverityAug project.

To install this package in R:

```
devtools::install_github("https://github.com/covidclinical/Phase2.1SeverityAugRPackage", subdir="FourCePhase2.1SeverityAug", upgrade=FALSE)
```
To run, use the following command: 

```
FourCePhase2.1SeverityAug::runAnalysis(chartReview_path = 'YOUR PATH HERE', maxdays = 30, mindays = -2, outcome = 'Severe?')
FourCePhase2.1SeverityAug::submitAnalysis()
```
The runAnalysis function has 4 arguments: 

- chartReview_path: location of chart-review data maxdays: maximum length of admissions to consider (default value: 30) 

- mindays: days prior to admission from which to collect events (default value: -2) 

- outcome: one of "Severe?" or "COVID-19 Hospitalization?"

The chart-review data should be a comma delimited file with 3 columns: "patient_num", "COVID-19 Hospitalization?", and "Severe?"

The latter 2 columns should be binary 1/0 based on the chart reviewed assessment of the patient admission.

patient_num | COVID-19 Hospitalization? | Severe?
------------ | ------------- | -------------
231 | 1 | 1
542 | 1 | 0
