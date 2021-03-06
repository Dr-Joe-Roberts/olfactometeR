% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarise_four_arm.R
\name{summarise_four_arm}
\alias{summarise_four_arm}
\title{Summarise four-arm olfactometer experiment results}
\usage{
summarise_four_arm()
}
\value{
\code{summarise_four_arm} returns a summary table of the experiment
replicates to the console, which can be exported as a .xlsx file if required.
}
\description{
\code{summarise_four_arm} allows the user to summarise replicates
from four-arm olfactometer experiments.

Upon executing \code{summarise_four_arm} the user will be prompted to select the
experiment replicates they want to summarise, displaying a summary table in the
console that can be exported as a .xlsx file if required.
}
\examples{
\dontrun{
library(olfactometeR)
summarise_four_arm()

Number of treatment arms (1/2): 2

                   Four-arm olfactometer: two treatment arms w/ two treatments
-----------------------------------------------------------------------------------------------
              Time spent in zone (secs)                                    Mean      Std. Error
            -----------------------------------------------------------------------------------
  Replicate   Centre   Treatment 1   Treatment 2   Control 1   Control 2   Control    Control
-----------------------------------------------------------------------------------------------
      1       56.32       0.00          36.28        0.00        23.20      11.60      11.60
      2       35.81       0.00          22.46        0.00        5.86       2.93        2.93
      3       44.79       24.77         0.00         27.74       0.00       13.87      13.87
-----------------------------------------------------------------------------------------------
  Study species: Myzus persicae
  Treatment 1: Linalool
  Treatment 2: Limonene

Save the ouput as an .xlsx file? (y/n) n
[1] "Output has not been saved"
}

}
