
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img width="120px" alt="olfactometeR logo" align="right" src="man/figures/logo.png">

# `olfactometeR` - Streamlined data collection for olfactometer behavioural experiments

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/Dr-Joe-Roberts/olfactometeR.svg?branch=master)](https://travis-ci.org/Dr-Joe-Roberts/olfactometeR)
<!-- badges: end -->

`olfactometeR` provides interactive, easy to use functions to facilitate
data collection for experiments that use olfactometers to test
behavioural responses of test subjects to volatile chemical stimuli.
This package was largely written for undergraduate students and
entomology researchers in the [Bruce
Lab](https://www.keele.ac.uk/lifesci/ourpeople/tobybruce/#research-and-scholarship)
at Keele University to replace the outdated software packages that do
not run on modern operating systems.

**DISCLAIMER:** `olfactometeR` is under active development and not all
features are optimised or available at present.

## Installation

You can install the development version of olfactometeR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Dr-Joe-Roberts/olfactometeR")
```

## Using `olfactometeR`

> Until a more stable release of `olfactometeR` please fork this repo
> and use your version to ensure some semblance of stability. Changes
> made here are likely to be sporadic but drastic. Otherwise, use this
> repo at your own discression\!

Currently there are two functions available for data collection, which
allow the user to interact with the console to record the behavioural
response of the study subject.

#### 1\. `record_four_arm()` for experiments using four-arm olfactometers

Four-arm olfactometers are conventionally split into five zones, one for
each arm as well as a central zone, with each zone corresponding to a
numerical key from `1:5`. When a study subject leaves a zone, the user
must use the numerical key corresponding to the departed zone to record
the correct zone. When the observation period is over, the user can end
the recording process by pressing `t`. At present the
`record_four_arm()` function is able to accomodate up to two treatment
arms.

Below is a data collection example for a four-arm olfactometer with one
treatment arm:

    # One treatment arm
    
    library(olfactometeR)
    
    record_four_arm()
    
    User initials: JR
    Year: 2019
    Experiment number: 1
    Replicate number: 1
    Number of treatment arms (1/2): 1
    Olfactometer arm containing treatment (1/2/3/4): 2
    Press any key to begin collecting data:
    Olfactometer zone: 5
    98.08 sec elapsed
    Olfactometer zone: 4
    25.61 sec elapsed
    Olfactometer zone: 2
    106.95 sec elapsed
    Olfactometer zone: 4
    3.57 sec elapsed
    Olfactometer zone: 2
    11.85 sec elapsed
    Olfactometer zone: 5
    11.37 sec elapsed
    Olfactometer zone: 3
    55.02 sec elapsed
    Olfactometer zone: 2
    11.72 sec elapsed
    Olfactometer zone: t
    
    
    |Olfactometer Zone|Time in Zone (secs)|Time in Zone (mins)|No. Times Zone Entered|Treatment Arm|
    |:---------------:|:-----------------:|:-----------------:|:--------------------:|:-----------:|
    |        1        |        0.00       |       0.00        |          0           |             |
    |        2        |      130.52       |       2.18        |          3           |      T      |
    |        3        |       55.02       |       0.92        |          1           |             |
    |        4        |       29.18       |       0.49        |          2           |             |
    |        5        |      109.45       |       1.82        |          2           |             |

#### 2\. `record_y_tube()` for experiments using Y-tube olfactometers

Each Y-tube olfactometer arm corresponds to a numerical key, either `1`
or `2`. When a study subject enters an olfactometer arm and crosses the
pre-determined line to indicate a decision has been made the user must
use the numerical key corresponding to the entered olfactometer arm to
record the individual as have made a choice. Recording will
automatically end once a choice has been made and entered into the
console.

Below is a data collection example for a Y-tube olfactometer:

    library(olfactometeR)
    
    record_y_tube()
    
    User initials: JR
    Year: 2019
    Experiment number: 1
    Replicate number: 1
    Olfactometer arm containing treatment: 2
    Press any key to begin recording data:
    Olfactometer zone: 1
    99.97 sec elapsed
    
    
    |Olfactometer Arm|Time to Reach Arm End (secs)|Time to Reach Arm End (mins)|Treatment Arm|
    |:--------------:|:--------------------------:|:--------------------------:|:-----------:|
    |        1       |            99.97           |             1.67           |             |
    |        2       |              NA            |              NA            |      T      |
