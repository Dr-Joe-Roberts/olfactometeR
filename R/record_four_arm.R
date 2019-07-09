#' Record movement data from four-arm olfactometer experiments
#' @name record_four_arm
#'
#' @description \code{record_four_arm} allows the user to interactively record the movements
#' of their study subject in a four-arm olfactometer.
#'
#' Upon executing \code{record_four_arm} the user will be prompted in the console to enter the
#' following information before recording: User initials', 'Year of experiment',
#' 'Experiment number', 'Replicate number', 'Number of treatment arms' and
#' 'Olfactometer arm containing treatment'.
#'
#' Four-arm olfactometers are typically divided into five unique zones (one for each arm
#' as well as a central zone), with each zone corresponding to a numerical
#' key from one to five. When a study subject leaves a zone, the user must use the
#' numerical key corresponding to the departed zone to record the correct zone. When the
#' observation period is over, the user can terminate the recording process by pressing 't'.
#'
#' @usage record_four_arm()
#'
#' @return \code{record_four_arm} returns a summary table of a study subject's movements
#' recorded in a four-arm olfactometer to the console and exports a .xlsx file containing
#' these values to the user's working directory.
#'
#' @examples
#' \dontrun{
#'
#' # One treatment arm
#'
#' Library(olfactometeR)
#'
#' record_four_arm()
#' User initials: JR
#' Year: 2019
#' Experiment number: 1
#' Replicate number: 1
#' Number of treatment arms (1/2): 1
#' Olfactometer arm containing treatment (1/2/3/4): 2
#' Press any key to begin collecting data:
#' Olfactometer zone: 5
#' 98.08 sec elapsed
#' Olfactometer zone: 4
#' 25.61 sec elapsed
#' Olfactometer zone: 2
#' 106.95 sec elapsed
#' Olfactometer zone: 4
#' 3.57 sec elapsed
#' Olfactometer zone: 2
#' 11.85 sec elapsed
#' Olfactometer zone: 5
#' 11.37 sec elapsed
#' Olfactometer zone: 3
#' 55.02 sec elapsed
#' Olfactometer zone: 2
#' 11.72 sec elapsed
#' Olfactometer zone: t
#'
#'
#' |Olfactometer Zone|Time in Zone (secs)|Time in Zone (mins)|No. Times Zone Entered|Treatment Arm|
#' |:---------------:|:-----------------:|:-----------------:|:--------------------:|:-----------:|
#' |        1        |        0.00       |       0.00        |          0           |             |
#' |        2        |      130.52       |       2.18        |          3           |      T      |
#' |        3        |       55.02       |       0.92        |          1           |             |
#' |        4        |       29.18       |       0.49        |          2           |             |
#' |        5        |      109.45       |       1.82        |          2           |             |
#'
#'
#' # Two treatment arms
#'
#' library(olfactometeR)
#'
#' record_four_arm()
#' User initials: JR
#' Year: 2019
#' Experiment number: 1
#' Replicate number: 1
#' Number of treatment arms (1/2): 2
#' Olfactometer arm containing treatment one (1/2/3/4): 1
#' Olfactometer arm containing treatment two (1/2/3/4): 4
#' Press any key to begin collecting data:
#' Olfactometer zone: 5
#' 33.94 sec elapsed
#' Olfactometer zone: 4
#' 23.4 sec elapsed
#' Olfactometer zone: 5
#' 5.91 sec elapsed
#' Olfactometer zone: 1
#' 42.33 sec elapsed
#' Olfactometer zone: 5
#' 28.64 sec elapsed
#' Olfactometer zone: 3
#' 8.36 sec elapsed
#' Olfactometer zone: t
#'
#'
#' |Olfactometer Zone|Time in Zone (secs)|Time in Zone (mins)|No. Times Zone Entered|Treatment Arms|
#' |:---------------:|:-----------------:|:-----------------:|:--------------------:|:------------:|
#' |        1        |       42.33       |        0.71       |           1          |       T      |
#' |        2        |        0.00       |        0.00       |           0          |              |
#' |        3        |        8.36       |        0.14       |           1          |              |
#' |        4        |       23.40       |        0.39       |           1          |       T      |
#' |        5        |       68.49       |        1.14       |           3          |              |
#'
#' }
#'
#' @export
#'
record_four_arm <- function() {
  user <- readline("User initials: ")

  year <- readline("Year: ")

  experiment <- readline("Experiment number: ")

  replicate <- readline("Replicate number: ")

  no_treatment_arms <- readline("Number of treatment arms (1/2): ")

  if (no_treatment_arms == 1){
    treatment_arm <- readline("Olfactometer arm containing treatment (1/2/3/4): ")

  start_timer <- readline("Press any key to begin collecting data: ")


  while (TRUE) {
    tictoc::tic()
    olfactometer_zone <- readline("Olfactometer zone: ")
    if (olfactometer_zone %in% 1:5) {
      elapsed <- tictoc::toc()
      utils::write.table(
        cbind(
          experiment,
          replicate,
          treatment_arm,
          olfactometer_zone,
          elapsed$toc - elapsed$tic
        ),
        file = paste(
          user,
          year,
          experiment,
          replicate,
          "Four_Arm_Olfactometer_Recording.txt",
          sep = "_"
        ),
        col.names = FALSE,
        row.names = FALSE,
        quote = FALSE,
        append = TRUE
      )
    } else if (olfactometer_zone == "t") {
      break
    } else if (olfactometer_zone < 1 |
      olfactometer_zone > 5 &
        olfactometer_zone != "t") {
      print("That is not a valid zone, please use numerical keys 1:5 to select valid zones")
    }
  }

  data <- readr::read_delim(
    paste(
      user,
      year,
      experiment,
      replicate,
      "Four_Arm_Olfactometer_Recording.txt",
      sep = "_"
    ),
    delim = " ",
    col_names = c("A", "B", "C", "D", "E"),
    col_types = readr::cols("D" = readr::col_integer())
  )

  data <- data %>%
    tidyr::complete(tidyr::nesting(A, B, C), D = seq(1, 5, 1L)) %>%
    dplyr::arrange(is.na(E)) %>%
    dplyr::mutate(E = tidyr::replace_na(E, 0))

  times_entered <- data %>%
    tidyr::complete(tidyr::nesting(A, B, C), D = seq(1, 5, 1L)) %>%
    dplyr::arrange(is.na(E)) %>%
    dplyr::mutate(E = tidyr::replace_na(E, 0)) %>%
    dplyr::add_count(D) %>%
    dplyr::mutate(n = (E != 0) * n) %>%
    dplyr::group_by(D) %>%
    dplyr::summarise_at(dplyr::vars(n), .funs = mean) %>%
    dplyr::pull(n)

  times_entered <- tibble::tibble(times_entered)

  zones <- data %>%
    dplyr::mutate(control = D != C)

  by_zone <- dplyr::group_by(zones, D)

  sum_zone_times <- dplyr::summarise(by_zone, time_secs = sum(E)) %>%
    dplyr::mutate(time_mins = time_secs / 60)

  treatment_zone <- zones %>%
    dplyr::filter(control == FALSE) %>%
    dplyr::mutate("Olfactometer Zone" = D) %>%
    dplyr::mutate(Treatment = sum(E)) %>%
    dplyr::mutate("Treatment Arm" = "T")

  tbl_one <- treatment_zone %>%
    dplyr::select("Olfactometer Zone", "Treatment Arm")

  control_zones <- zones %>%
    dplyr::filter(control == TRUE) %>%
    dplyr::mutate("Olfactometer Zone" = D) %>%
    dplyr::mutate(Treatment = sum(E)) %>%
    dplyr::mutate("Treatment Arm" = " ")

  tbl_two <- control_zones %>%
    dplyr::select("Olfactometer Zone", "Treatment Arm")

  results_tbl <- dplyr::bind_rows(tbl_one, tbl_two) %>%
    dplyr::distinct()

  ordered_zones <- dplyr::arrange(results_tbl, results_tbl$`Olfactometer Zone`)

  results_table <- dplyr::bind_cols(sum_zone_times, times_entered, ordered_zones)

  results_table <- results_table %>%
    dplyr::select(D, time_secs, time_mins, times_entered, `Treatment Arm`) %>%
    dplyr::rename("Olfactometer Zone" = D) %>%
    dplyr::rename("Total Time in Zone (secs)" = time_secs) %>%
    dplyr::rename("Total Time in Zone (mins)" = time_mins) %>%
    dplyr::rename("No. of Times Zone Entered" = times_entered)

  final_table <- knitr::kable(
    utils::head(results_table),
    format = "markdown",
    digits = 2,
    align = "c"
  )

  base::print(final_table)

  rio::export(
    results_table,
    paste(
      user,
      year,
      experiment,
      replicate,
      "Four_Arm_Olfactometer_Recording_Summary.xlsx",
      sep = "_"
    )
  )
 }

  else if (no_treatment_arms == 2) {
    treatment_arm_one <- readline("Olfactometer arm containing treatment one (1/2/3/4): ")

    treatment_arm_two <- readline("Olfactometer arm containing treatment two (1/2/3/4): ")

    start_timer <- readline("Press any key to begin collecting data: ")

    while (TRUE) {
      tictoc::tic()
      olfactometer_zone <- readline("Olfactometer zone: ")
      if (olfactometer_zone %in% 1:5) {
        elapsed <- tictoc::toc()
        utils::write.table(
          cbind(
            experiment,
            replicate,
            treatment_arm_one,
            treatment_arm_two,
            olfactometer_zone,
            elapsed$toc - elapsed$tic
          ),
          file = paste(
            user,
            year,
            experiment,
            replicate,
            "Four_Arm_Olfactometer_Recording.txt",
            sep = "_"
          ),
          col.names = FALSE,
          row.names = FALSE,
          quote = FALSE,
          append = TRUE
        )
      } else if (olfactometer_zone == "t") {
        break # break out of while loop
      } else if (olfactometer_zone < 1 |
        olfactometer_zone > 5 &
          olfactometer_zone != "t") {
        print("That is not a valid zone, please use numerical keys 1:5 to select valid zones")
      }
    }

    data <- readr::read_delim(
      paste(
        user,
        year,
        experiment,
        replicate,
        "Four_Arm_Olfactometer_Recording.txt",
        sep = "_"
      ),
      delim = " ",
      col_names = c("A", "B", "C", "D", "E", "G"),
      col_types = readr::cols("E" = readr::col_integer())
    )

    data <- data %>%
      tidyr::complete(tidyr::nesting(A, B, C, D), E = seq(1, 5, 1L)) %>%
      dplyr::arrange(is.na(G)) %>%
      dplyr::mutate(G = tidyr::replace_na(G, 0))

    times_entered <- data %>%
      tidyr::complete(tidyr::nesting(A, B, C, D), E = seq(1, 5, 1L)) %>%
      dplyr::arrange(is.na(G)) %>%
      dplyr::mutate(G = tidyr::replace_na(G, 0)) %>%
      dplyr::add_count(E) %>%
      dplyr::mutate(n = (G != 0) * n) %>%
      dplyr::group_by(E) %>%
      dplyr::summarise_at(dplyr::vars(n), .funs = mean) %>%
      dplyr::pull(n)

    times_entered <- tibble::tibble(times_entered)

    zones <- data %>%
      dplyr::mutate(treatment = E %in% c(C, D))

    by_zone <- dplyr::group_by(zones, E)

    sum_zone_times <- dplyr::summarise(by_zone, time_secs = sum(G)) %>%
      dplyr::mutate(time_mins = time_secs / 60)

    treatment_zone <- zones %>%
      dplyr::filter(treatment == TRUE) %>%
      dplyr::mutate("Olfactometer Zone" = E) %>%
      dplyr::mutate(Treatment = sum(G)) %>%
      dplyr::mutate("Treatment Arms" = "T")

    tbl_one <- treatment_zone %>%
      dplyr::select("Olfactometer Zone", "Treatment Arms")

    control_zones <- zones %>%
      dplyr::filter(treatment == FALSE) %>%
      dplyr::mutate("Olfactometer Zone" = E) %>%
      dplyr::mutate(Treatment = sum(G)) %>%
      dplyr::mutate("Treatment Arms" = " ")

    tbl_two <- control_zones %>%
      dplyr::select("Olfactometer Zone", "Treatment Arms")

    results_tbl <- dplyr::bind_rows(tbl_one, tbl_two) %>%
      dplyr::distinct()

    ordered_zones <- dplyr::arrange(results_tbl, results_tbl$`Olfactometer Zone`)

    results_table <- dplyr::bind_cols(sum_zone_times, times_entered, ordered_zones)

    results_table <- results_table %>%
      dplyr::select(E, time_secs, time_mins, times_entered, `Treatment Arms`) %>%
      dplyr::rename("Olfactometer Zone" = E) %>%
      dplyr::rename("Total Time in Zone (secs)" = time_secs) %>%
      dplyr::rename("Total Time in Zone (mins)" = time_mins) %>%
      dplyr::rename("No. of Times Zone Entered" = times_entered)

    final_table <- knitr::kable(
      utils::head(results_table),
      format = "markdown",
      digits = 2,
      align = "c"
    )

    base::print(final_table)

    rio::export(
      results_table,
      paste(
        user,
        year,
        experiment,
        replicate,
        "Four_Arm_Olfactometer_Recording_Summary.xlsx",
        sep = "_"
      )
    )
  }
}
