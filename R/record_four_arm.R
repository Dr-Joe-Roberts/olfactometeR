#' Record behavioural data from four-arm olfactometer experiments
#' @name record_four_arm
#'
#' @description \code{record_four_arm} allows the user to interactively record the movements
#' of their study subject in a four-arm olfactometer.
#'
#' Upon executing \code{record_four_arm} the user will be prompted in the console to enter the
#' following information before recording: User initials', 'Year of experiment',
#' 'Experiment number', 'Replicate number', 'Centre zone assignment', 'Number
#' of treatment arms' and Olfactometer arm containing treatment'.
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
#' recorded in a four-arm olfactometer to the console.
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
#' Number of treatment arms (1:2): 1
#' Olfactometer arm containing treatment (1:4): 2
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
#' Number of treatment arms (1:2): 2
#' Olfactometer arm containing treatment one (1:4): 1
#' Olfactometer arm containing treatment two (1:4): 4
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

  species <- readline("Study species being tested: ")

  experiment <- readline("Experiment number: ")

  replicate <- readline("Replicate number: ")

  central_zone <- readline("Centre zone (1:5): ")

  no_treatment_arms <- readline("Number of treatment arms (1:2): ")

  if (no_treatment_arms == 1) {
    treatment_arm <- readline("Olfactometer arm containing treatment (1:5): ")

    treatment_ID <- readline("Treatment: ")

    variable_table <- tibble::tibble(
      "User initials" = user,
      "Study year" = year,
      "Study subject species" = species,
      "Experiment no." = experiment,
      "Replicate no." = replicate,
      "Centre zone assignment" = central_zone,
      "Treatment arm assignment" = treatment_arm,
      "Treatment" = treatment_ID
    )

    variable_table <- tidyr::gather(variable_table, "Variable", "User Response")

    variable_table <- knitr::kable(
      variable_table,
      format = "markdown",
      digits = 0,
      align = "l"
    )

    base::print(variable_table)

    user_check <- readline("Are the entered details correct (y/n): ")

    if (user_check == "y") {
      start_timer <- readline("Press any key to begin data collection: ")


      while (TRUE) {
        tictoc::tic()
        olfactometer_zone <- readline("Olfactometer zone: ")
        if (olfactometer_zone %in% 1:5) {
          elapsed <- tictoc::toc()
          utils::write.table(
            cbind(
              experiment,
              replicate,
              species,
              central_zone,
              treatment_arm,
              treatment_ID,
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
        col_names = c("A", "B", "C", "D", "E", "G", "H", "I"),
        col_types = readr::cols("H" = readr::col_integer())
      )

      data <- data %>%
        tidyr::complete(tidyr::nesting(A, B, C, D, E, G), H = seq(1, 5, 1L)) %>%
        dplyr::arrange(is.na(I)) %>%
        dplyr::mutate(I = tidyr::replace_na(I, 0))

      times_entered <- data %>%
        tidyr::complete(tidyr::nesting(A, B, C, D, E, G), H = seq(1, 5, 1L)) %>%
        dplyr::arrange(is.na(I)) %>%
        dplyr::mutate(I = tidyr::replace_na(I, 0)) %>%
        dplyr::add_count(H) %>%
        dplyr::mutate(n = (I != 0) * n) %>%
        dplyr::group_by(H) %>%
        dplyr::summarise_at(dplyr::vars(n), .funs = mean) %>%
        dplyr::pull(n)

      times_entered <- tibble::tibble(times_entered)

      zones <- data %>%
        dplyr::mutate(control = H != E) %>%
        dplyr::mutate(arms = H != D)

      by_zone <- dplyr::group_by(zones, H)

      sum_zone_times <- dplyr::summarise(by_zone, time_secs = sum(I)) %>%
        dplyr::mutate(time_mins = time_secs / 60)

      centre_zone <- zones %>%
        dplyr::filter(arms == FALSE) %>%
        dplyr::mutate("Olfactometer Zone" = H) %>%
        dplyr::mutate(Centre = sum(I)) %>%
        dplyr::mutate("Zone Assignment" = "Centre")

      tbl_zero <- centre_zone %>%
        dplyr::select("Olfactometer Zone", "Zone Assignment")

      treatment_zone <- zones %>%
        dplyr::filter(control == FALSE) %>%
        dplyr::mutate("Olfactometer Zone" = H) %>%
        dplyr::mutate(Treatment = sum(I)) %>%
        dplyr::mutate("Zone Assignment" = "Treatment")

      tbl_one <- treatment_zone %>%
        dplyr::select("Olfactometer Zone", "Zone Assignment")

      control_zones <- zones %>%
        dplyr::filter(control == TRUE & arms == TRUE) %>%
        dplyr::mutate("Olfactometer Zone" = H) %>%
        dplyr::mutate(Treatment = sum(I)) %>%
        dplyr::mutate("Zone Assignment" = "Control")

      tbl_two <- control_zones %>%
        dplyr::select("Olfactometer Zone", "Zone Assignment")

      results_tbl <- dplyr::bind_rows(tbl_zero, tbl_one, tbl_two) %>%
        dplyr::distinct()

      ordered_zones <- dplyr::arrange(results_tbl, results_tbl$`Olfactometer Zone`)

      results_table <- dplyr::bind_cols(sum_zone_times, times_entered, ordered_zones)

      results_table <- results_table %>%
        dplyr::select(H, `Zone Assignment`, time_secs, time_mins, times_entered) %>%
        dplyr::rename("Olfactometer Zone" = H) %>%
        dplyr::rename("Total Time in Zone (secs)" = time_secs) %>%
        dplyr::rename("Total Time in Zone (mins)" = time_mins) %>%
        dplyr::rename("No. of Times Zone Entered" = times_entered)

      final_table <- knitr::kable(
        results_table,
        format = "markdown",
        digits = 2,
        align = "c"
      )

      base::print(final_table)

      file_export <- readline("Save the ouput as an .xlsx file? (y/n) ")

      if (file_export == "y") {
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
      } else if (file_export == "n") {
        print("Output has not been saved as an .xlsx file")
      }
    }

    else if (user_check == "n") {
      print("Variables are incorrect, please execute the function again to restart data collection")
    }
  }

  else if (no_treatment_arms == 2) {
    treatment_arm_one <- readline("Olfactometer arm containing treatment one (1:5): ")

    treatment_ID_one <- readline("Treatment one: ")

    treatment_arm_two <- readline("Olfactometer arm containing treatment two (1:5): ")

    treatment_ID_two <- readline("Treatment two name: ")

    variable_table <- tibble::tibble(
      "User initials" = user,
      "Study year" = year,
      "Study subject species" = species,
      "Experiment no." = experiment,
      "Replicate no." = replicate,
      "Centre zone assignment" = central_zone,
      "Treatment 1 arm assignment" = treatment_arm_one,
      "Treatment 1" = treatment_ID_one,
      "Treatment 2 arm assignment" = treatment_arm_two,
      "Treatment 2" = treatment_ID_two
    )

    variable_table <- tidyr::gather(variable_table, "Variable", "User Response")

    variable_table <- knitr::kable(
      variable_table,
      format = "markdown",
      digits = 0,
      align = "l"
    )

    base::print(variable_table)

    user_check <- readline("Are the entered details correct (y/n): ")

    if (user_check == "y") {
      start_timer <- readline("Press any key to begin data collection: ")

      while (TRUE) {
        tictoc::tic()
        olfactometer_zone <- readline("Olfactometer zone: ")
        if (olfactometer_zone %in% 1:5) {
          elapsed <- tictoc::toc()
          utils::write.table(
            cbind(
              experiment,
              replicate,
              species,
              central_zone,
              treatment_arm_one,
              treatment_ID_one,
              treatment_arm_two,
              treatment_ID_two,
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
        col_names = c("A", "B", "C", "D", "E", "G", "H", "I", "J", "K"),
        col_types = readr::cols("J" = readr::col_integer())
      )

      data <- data %>%
        tidyr::complete(tidyr::nesting(A, B, C, D, E, G, H, I), J = seq(1, 5, 1L)) %>%
        dplyr::arrange(is.na(K)) %>%
        dplyr::mutate(K = tidyr::replace_na(K, 0))

      times_entered <- data %>%
        tidyr::complete(tidyr::nesting(A, B, C, D, E, G, H, I), J = seq(1, 5, 1L)) %>%
        dplyr::arrange(is.na(K)) %>%
        dplyr::mutate(K = tidyr::replace_na(K, 0)) %>%
        dplyr::add_count(J) %>%
        dplyr::mutate(n = (K != 0) * n) %>%
        dplyr::group_by(J) %>%
        dplyr::summarise_at(dplyr::vars(n), .funs = mean) %>%
        dplyr::pull(n)

      times_entered <- tibble::tibble(times_entered)

      zones <- data %>%
        dplyr::mutate(treatment = J %in% c(E, H)) %>%
        dplyr::mutate(treatment_one = J == E) %>%
        dplyr::mutate(treatment_two = J == H) %>%
        dplyr::mutate(arms = J != D)

      by_zone <- dplyr::group_by(zones, J)

      sum_zone_times <- dplyr::summarise(by_zone, time_secs = sum(K)) %>%
        dplyr::mutate(time_mins = time_secs / 60)

      centre_zone <- zones %>%
        dplyr::filter(arms == FALSE) %>%
        dplyr::mutate("Olfactometer Zone" = J) %>%
        dplyr::mutate(Centre = sum(K)) %>%
        dplyr::mutate("Zone Assignment" = "Centre")

      tbl_zero <- centre_zone %>%
        dplyr::select("Olfactometer Zone", "Zone Assignment")

      treatment_one <- zones %>%
        dplyr::filter(treatment_one == TRUE) %>%
        dplyr::mutate("Olfactometer Zone" = J) %>%
        dplyr::mutate(Treatment = sum(K)) %>%
        dplyr::mutate("Zone Assignment" = "Treatment 1")

      tbl_one <- treatment_one %>%
        dplyr::select("Olfactometer Zone", "Zone Assignment")

      treatment_two <- zones %>%
        dplyr::filter(treatment_two == TRUE) %>%
        dplyr::mutate("Olfactometer Zone" = J) %>%
        dplyr::mutate(Treatment = sum(K)) %>%
        dplyr::mutate("Zone Assignment" = "Treatment 2")

      tbl_two <- treatment_two %>%
        dplyr::select("Olfactometer Zone", "Zone Assignment")

      control_zones <- zones %>%
        dplyr::filter(treatment == FALSE & arms == TRUE) %>%
        dplyr::mutate("Olfactometer Zone" = J) %>%
        dplyr::mutate(Treatment = sum(K)) %>%
        dplyr::mutate("Zone Assignment" = "Control")

      tbl_three <- control_zones %>%
        dplyr::select("Olfactometer Zone", "Zone Assignment")

      results_tbl <- dplyr::bind_rows(tbl_zero, tbl_one, tbl_two, tbl_three) %>%
        dplyr::distinct()

      ordered_zones <- dplyr::arrange(results_tbl, results_tbl$`Olfactometer Zone`)

      results_table <- dplyr::bind_cols(sum_zone_times, times_entered, ordered_zones)

      results_table <- results_table %>%
        dplyr::select(J, "Zone Assignment", time_secs, time_mins, times_entered) %>%
        dplyr::rename("Olfactometer Zone" = J) %>%
        dplyr::rename("Total Time in Zone (secs)" = time_secs) %>%
        dplyr::rename("Total Time in Zone (mins)" = time_mins) %>%
        dplyr::rename("No. of Times Zone Entered" = times_entered)

      final_table <- knitr::kable(
        results_table,
        format = "markdown",
        digits = 2,
        align = "c"
      )

      base::print(final_table)

      file_export <- readline("Save the ouput as an .xlsx file? (y/n) ")

      if (file_export == "y") {
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
      } else if (file_export == "n") {
        print("File has not been saved as an .xlsx file")
      }
    }
    else if (user_check == "n") {
      print("Variables are incorrect, please execute the function again to restart data collection")
    }
  }
}
