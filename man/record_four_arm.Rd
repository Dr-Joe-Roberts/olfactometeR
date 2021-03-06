% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/record_four_arm.R
\name{record_four_arm}
\alias{record_four_arm}
\title{Record behavioural data from four-arm olfactometer experiments}
\usage{
record_four_arm()
}
\value{
\code{record_four_arm} returns a summary table of a study subject's movements
recorded in a four-arm olfactometer to the console.
}
\description{
\code{record_four_arm} allows the user to interactively record the movements
of their study subject in a four-arm olfactometer.

Upon executing \code{record_four_arm} the user will be prompted in the console to enter the
following information before recording: User initials', 'Year of experiment',
'Experiment number', 'Replicate number', 'Centre zone assignment', 'Number
of treatment arms' and Olfactometer arm containing treatment'.

Four-arm olfactometers are typically divided into five unique zones (one for each arm
as well as a central zone), with each zone corresponding to a numerical
key from one to five. When a study subject leaves a zone, the user must use the
numerical key corresponding to the departed zone then enter to record the correct zone. When
the observation period is over, the user can terminate the recording process by pressing 't'
then enter.
}
\examples{
\dontrun{

# One treatment arm

library(olfactometeR)
record_four_arm()

User initials: JR
Year: 2019
Study species being tested: "Myzus persicae"       ## Must be entered between quotation marks!
Experiment number: 1
Replicate number: 1
Centre zone (1:5): 5
Number of treatment arms (1:2): 1
Olfactometer arm containing treatment (1:5): 2
Treatment: "(E)-beta-farnesene (0.1 mg/ml)"        ## Must be entered between quotation marks!


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
-----------------------------------------------------------------------------------------------
Olfactometer Zone   Zone Assignment   Total Time (secs)   Total Time (mins)   No. Times Entered
-----------------------------------------------------------------------------------------------
        1               Control             25.70               0.43                  1
        2              Treatment            4.48                0.07                  2
        3               Control             22.52               0.38                  2
        4               Control            251.64               4.19                  1
        5               Centre              58.84               0.98                  3
-----------------------------------------------------------------------------------------------
  Study species: Myzus persicae
  Treatment: (E)-beta-farnesene (0.1 mg/ml)

Save the ouput as an .xlsx file? (y/n) n
[1] "Output has not been saved"
}

}
