
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img width="120px" alt="olfactometeR logo" align="right" src="man/figures/logo.png">

# `olfactometeR` - Streamlined data collection for olfactometer experiments

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/Dr-Joe-Roberts/olfactometeR.svg?branch=master)](https://travis-ci.org/Dr-Joe-Roberts/olfactometeR)
<!-- badges: end -->

The `olfactometeR` package provides interactive, easy to use functions
that facilitate data collection from olfactometer experiments. This
package was largely written for entomology students and researchers in
the [Bruce
Lab](https://www.keele.ac.uk/lifesci/ourpeople/tobybruce/#research-and-scholarship)
at Keele University to replace the outdated software packages available
for MS-DOS.

**DISCLAIMER**: `olfactometeR` is under active development and not all
features are optimised or available at present.

## Installation

You can install the development version of olfactometeR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Dr-Joe-Roberts/olfactometeR")
```

## Usage

Currently there are two primary functions that facilitate data
acquisition from four-arm olfactometer experiments where there is only
**one** treatment arm. Further olfactometer designs and functions are in
development. The two functions available are:

  - `record_data()` for data acquisition
  - `results_table()` for viewing a summary results table in the console
    and exporting results as a .xlsx file

Executing the `record_data()` function will prompt the user to input the
following information:

  - User initials
  - Year
  - Experiment number
  - Replicate number
  - Olfactometer arm containing treatment

Once these details have been supplied the user will be prompted to press
‘s’ to begin recording data. Data acquisition requires the user to press
the number key corresponding to the olfactometer zone that the test
subject was in once it leaves that zone - number keys 1 to 5 are valid
for olfactometer zones. To end data acquisition, the user must press ‘t’
to terminate data acquisition.

## Examples

Below is a basic data acquisition example:

    record_data()
    
    User initials: JR
    Year: 2019
    Experiment number: 1
    Replicate number: 1
    Olfactometer arm containing treatment: 2
    Press s to begin recording data: s
    Olfactometer zone: 4
    13.7 sec elapsed
    Olfactometer zone: 2
    6.19 sec elapsed
    Olfactometer zone: 5
    6.61 sec elapsed
    Olfactometer zone: 2
    45.67 sec elapsed
    Olfactometer zone: t
