
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img width="120px" alt="olfactometeR logo" align="right" src="man/figures/logo.png">

# `olfactometeR` - Streamlined data collection for olfactometer experiments

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/Dr-Joe-Roberts/olfactometeR.svg?branch=master)](https://travis-ci.org/Dr-Joe-Roberts/olfactometeR)
[![DOI](https://zenodo.org/badge/193857666.svg)](https://zenodo.org/badge/latestdoi/193857666)
<!-- badges: end -->

`olfactometeR` provides easy to use functions to facilitate data
collection for experiments using olfactometers to study the behavioural
responses of test subjects to volatile chemical stimuli under laboratory
conditions. This package was largely written for undergraduate students
and entomology researchers in the [Bruce Lab at Keele
University](https://www.keele.ac.uk/lifesci/ourpeople/tobybruce/#research-and-scholarship)
to replace outdated software programmes that do not run on modern
operating systems.

## Installation

You can install the development version of olfactometeR from
[GitHub](https://github.com/):

``` r
# install.packages("devtools")
devtools::install_github("Dr-Joe-Roberts/olfactometeR")
```

## Using `olfactometeR`

> Until the non-development version of `olfactometeR` is released please
> be aware that changes made here are likely to be sporadic but drastic.
> Use at your own discression\!

Currently `olfactometeR` has four functions that can be split into two
categories:

1.  Data collection
2.  Data processing

Data collection functions allow the user to interact with the R console
to record the behavioural response of their study subject in four-arm or
Y-tube olfactometer experiments:

  - `record_four_arm()`: collect data from four-arm olfactometers
  - `record_y_tube()`: collect data from Y-tube olfactometers

Data processing functions allow the user to combine multiple replicates
from the same experiment into a single output for data analysis and/or
visualisation:

  - `summarise_four_arm()`
  - `summarise_y_tube()`

## Examples

##### 1\. Four Arm Olfactometer

Executing the `record_four_arm()` function will first prompt the user to
enter a variety of experimental details that are required to ensure
accurate data collection and traceability. Both the ‘study species being
tested’ and ‘treatment’ details need to be entered between quotation
marks (e.g. `Treatment: "Linalool (0.1 mg/ml)"`). A summary table of the
experimental details will be displayed in the console to allow the user
to corroborate them prior to data collection.

Four-arm olfactometers are conventionally split into five zones, one for
each arm as well as a central zone, with each zone corresponding to a
numerical key from 1 to 5. When the study subject leaves a zone, the
user must press the numerical key assigned to the departed zone then
`Enter` for the time spent in the zone to be recorded. Once the
observation period is complete, the user can end data collection by
pressing `t` then `Enter`, which will display a summary results table
for the observation period in the console and prompt the user to save
the output as a .xlsx file. `record_four_arm()` can accomodate up to two
treatment arms.

###### Example:

    # One treatment arm
    
    library(olfactometeR)
    record_four_arm()
    
    User initials: JR
    Year: 2019
    Study species being tested: "Myzus persicae"                  ## Must be entered between quotation marks!
    Experiment number: 1
    Replicate number: 1
    Centre zone (1:5): 5
    Number of treatment arms (1:2): 1
    Olfactometer arm containing treatment (1:5): 2
    Treatment: "(E)-beta-farnesene (0.1 mg/ml)"                   ## Must be entered between quotation marks!
    
    
    |Variable                 |User Response                    |
    |:------------------------|:--------------------------------|
    |User initials            |JR                               |
    |Study year               |2019                             |
    |Study subject species    |"Myzus persicae"                 |
    |Experiment no.           |1                                |
    |Replicate no.            |1                                |
    |Centre zone assignment   |5                                |
    |Treatment arm assignment |2                                |
    |Treatment                |"(E)-beta-farnesene (0.1 mg/ml)" |
    
    Are the entered details correct (y/n): y
    
    Press any key to begin data collection: 
    Olfactometer zone: 5
    32.86 sec elapsed
    Olfactometer zone: 1
    25.7 sec elapsed
    Olfactometer zone: 5
    19.66 sec elapsed
    Olfactometer zone: 2
    3.21 sec elapsed
    Olfactometer zone: 3
    11.11 sec elapsed
    Olfactometer zone: 4
    251.64 sec elapsed
    Olfactometer zone: 3
    11.41 sec elapsed
    Olfactometer zone: 2
    1.27 sec elapsed
    Olfactometer zone: 5
    6.32 sec elapsed
    Olfactometer zone: t
    
                                 Four-arm olfactometer: one treatment arm                              
    ───────────────────────────────────────────────────────────────────────────────────────────────────
      Olfactometer Zone   Zone Assignment   Total Time (secs)   Total Time (mins)   No. Times Entered  
    ───────────────────────────────────────────────────────────────────────────────────────────────────
              1               Control             25.70               0.43                  1          
              2              Treatment            4.48                0.07                  2          
              3               Control             22.52               0.38                  2          
              4               Control            251.64               4.19                  1          
              5               Centre              58.84               0.98                  3          
    ───────────────────────────────────────────────────────────────────────────────────────────────────
      Study species: Myzus persicae                                                                    
      Treatment: (E)-beta-farnesene (0.1 mg/ml)                                                                   
    
    Save the ouput as an .xlsx file? (y/n) n
    [1] "Output has not been saved"

    library(olfactometeR)
    summarise_four_arm()
    
    Number of treatment arms (1/2): 2
    
                        Four-arm olfactometer: two treatment arms w/ two treatments
    ──────────────────────────────────────────────────────────────────────────────────────────────
                 Time spent in zone (secs)                                    Mean      Std. Error
               ───────────────────────────────────────────────────────────────────────────────────
       Replicate   Centre   Treatment 1   Treatment 2   Control 1   Control 2   Control    Control
    ──────────────────────────────────────────────────────────────────────────────────────────────
           1       56.32       0.00          36.28        0.00        23.20      11.60      11.60
           2       35.81       0.00          22.46        0.00        5.86       2.93        2.93
           3       44.79       24.77         0.00         27.74       0.00       13.87      13.87
    ──────────────────────────────────────────────────────────────────────────────────────────────
      Study species: Myzus persicae
      Treatment 1: Linalool
      Treatment 2: Limonene
    
    Save the ouput as an .xlsx file? (y/n) n
    [1] "Output has not been saved"

##### 2\. Y-Tube Olfactometer

Executing the `record_y_tube()` function will first prompt the user to
enter a variety of experimental details that are required to ensure
accurate data collection and traceability. Both the ‘study species being
tested’ and ‘treatment’ details need to be entered between quotation
marks (e.g. `Treatment: "Linalool (0.1 mg/ml)"`). A summary table of the
experimental details will be displayed in the console to allow the user
to corroborate them prior to data collection.

Each of the two Y-tube olfactometer arm corresponds to a numerical key,
either 1 or 2. When the study subject enters one of the olfactometer
arms and crosses the pre-determined line to indicate a decision has been
made, the user must press the numerical key assigned to the entered
olfactometer arm then `Enter` to record the individual as having made a
choice. Recording will automatically end once a choice has been entered
into the console, displaying a summary results table in the console.

###### Example:

    library(olfactometeR)
    
    record_y_tube()
    
    User initials: JR
    Year: 2019
    Study species being tested: "Myzus"                           ## Must be entered between quotation marks!
    Experiment number: 1
    Replicate number: 2
    Olfactometer arm containing treatment (1/2): 2
    Treatment: "Limonene (0.1 mg/ml)"                             ## Must be entered between quotation marks!
    
    
    |Variable                 |User Response          |
    |:------------------------|:----------------------|
    |User initials            |JR                     |
    |Study year               |2019                   |
    |Study subject species    |"Myzus"                |
    |Experiment no.           |1                      |
    |Replicate no.            |2                      |
    |Treatment arm assignment |2                      |
    |Treatment                |"Limonene (0.1 mg/ml)" |
    
    Are the entered details correct (y/n): y
    
    Press any key to begin recording data: 
    Olfactometer zone: 2
    3.33 sec elapsed
    
                                      Y-Tube olfactometer                                   
    ────────────────────────────────────────────────────────────────────────────────────────
      Olfactometer Arm   Zone Assignment   Time to Arm End (secs)   Time to Arm End (mins)  
    ────────────────────────────────────────────────────────────────────────────────────────
             1               Control                                                        
             2              Treatment               3.33                     0.06           
    ────────────────────────────────────────────────────────────────────────────────────────
      Study species: Myzus                                                                  
      Treatment: Limonene (0.1 mg/ml)                                                       
    
    Save the ouput (y/n): n
    [1] "Output has not been saved"

## The future :crystal\_ball:

There are a number of features I intend to develop for `olfactometeR` to
help streamline data analysis and visualisation for behavioural
experiments using olfactometry. Feature suggestions and issue reports
welcome\!
