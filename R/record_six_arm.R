#' Record behavioural data from six-arm olfactometer experiments
#' @name record_six_arm
#'
#' @description \code{record_six_arm} allows the user to interactively record the movements
#' of their study subject in a six-arm olfactometer.
#'
#' Upon executing \code{record_six_arm} the user will be prompted in the console to enter the
#' following information before recording: User initials', 'Year of experiment',
#' 'Experiment number', 'Replicate number', 'Centre zone assignment', 'Number of
#' treatment arms' and 'Olfactometer arm containing treatment'.
#'
#' Six-arm olfactometers are typically divided into seven unique zones (one for each arm
#' as well as a central zone), with each zone corresponding to a numerical
#' key from one to seven. When a study subject leaves a zone, the user must use the
#' numerical key corresponding to the departed zone to record the correct zone. When the
#' observation period is over, the user can terminate the recording process by pressing 't'.
#'
#' @usage record_six_arm()
#'
#' @return \code{record_six_arm} returns a summary table of a study subject's movements
#' recorded in a six-arm olfactometer to the console.
#'
#' @examples
#' \dontrun{
#'
#' # One treatment arm
#'
#' Library(olfactometeR)
#'
#' record_six_arm()
#' User initials: JR
#' Year: 2019
#' Experiment number: 1
#' Replicate number: 1
#' Centre zone (1:7): 7
#' Number of treatment arms (1/2): 1
#' Olfactometer arm containing treatment (1:7): 1
#' Press any key to begin recording data:
#' Olfactometer zone: 7
#' 17.58 sec elapsed
#' Olfactometer zone: 4
#' 17.39 sec elapsed
#' Olfactometer zone: 7
#' 20.77 sec elapsed
#' Olfactometer zone: 6
#' 27.27 sec elapsed
#' Olfactometer zone: 7
#' 6.44 sec elapsed
#' Olfactometer zone: 1
#' 39.47 sec elapsed
#' Olfactometer zone: t


#' |Olfactometer Zone|Time in Zone (secs)|Time in Zone (mins)|No. Times Zone Entered|Zone Assignment|
#' |:---------------:|:-----------------:|:-----------------:|:--------------------:|:-------------:|
#' |        1        |       39.47       |        0.66       |         1            |   Treatment   |
#' |        2        |        0.00       |        0.00       |         0            |    Control    |
#' |        3        |        0.00       |        0.00       |         0            |    Control    |
#' |        4        |       17.39       |        0.29       |         1            |    Control    |
#' |        5        |        0.00       |        0.00       |         0            |    Control    |
#' |        6        |       27.27       |        0.45       |         1            |    Control    |
#' |        7        |       44.79       |        0.75       |         3            |    Centre     |
#'
#'}
#' @export
#'
record_six_arm <- function() {
  user <- readline("User initials: ")

  year <- readline("Year: ")

  experiment <- readline("Experiment number: ")

  replicate <- readline("Replicate number: ")

  central_zone <- readline("Centre zone (1:7): ")

  no_treatment_arms <- readline("Number of treatment arms (1:2): ")

  if (no_treatment_arms == 1) {
    treatment_arm <- readline("Olfactometer arm containing treatment (1:7): ")

    start_timer <- readline("Press any key to begin recording data: ")

    while (TRUE) {
      # open infinite while loop
      tictoc::tic() # start timer
      olfactometer_zone <- readline("Olfactometer zone: ") # allow for entry of state
      if (olfactometer_zone %in% 1:7) { # check if it's acceptable
        elapsed <- tictoc::toc() # if it is then end timer and record data
        utils::write.table(
          cbind(
            experiment,
            replicate,
            central_zone,
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
      } else if (olfactometer_zone == "t") { # if input is 't'
        break # break out of while loop
      } else if (olfactometer_zone < 1 |
        olfactometer_zone > 7 &
          olfactometer_zone != "t") { # if input is not and accepted state AND is not 't'
        print("That is not a valid zone, please use numerical keys 1:7 to select valid zones")
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
      tidyr::complete(tidyr::nesting(A, B, C, D), E = seq(1, 7, 1L)) %>%
      dplyr::arrange(is.na(G)) %>%
      dplyr::mutate(G = tidyr::replace_na(G, 0))

    times_entered <- data %>%
      tidyr::complete(tidyr::nesting(A, B, C, D), E = seq(1, 7, 1L)) %>%
      dplyr::arrange(is.na(G)) %>%
      dplyr::mutate(G = tidyr::replace_na(G, 0)) %>%
      dplyr::add_count(E) %>%
      dplyr::mutate(n = (G != 0) * n) %>%
      dplyr::group_by(E) %>%
      dplyr::summarise_at(dplyr::vars(n), .funs = mean) %>%
      dplyr::pull(n)

    times_entered <- tibble::tibble(times_entered)

    zones <- data %>%
      dplyr::mutate(control = E != D) %>%
      dplyr::mutate(arms = E != C)

    by_zone <- dplyr::group_by(zones, E)

    sum_zone_times <- dplyr::summarise(by_zone, time_secs = sum(G)) %>%
      dplyr::mutate(time_mins = time_secs / 60)

    centre_zone <- zones %>%
      dplyr::filter(arms == FALSE) %>%
      dplyr::mutate("Olfactometer Zone" = E) %>%
      dplyr::mutate(Centre = sum(G)) %>%
      dplyr::mutate("Zone Assignment" = "Centre")

    tbl_zero <- centre_zone %>%
      dplyr::select("Olfactometer Zone", "Zone Assignment")

    treatment_zone <- zones %>%
      dplyr::filter(control == FALSE) %>%
      dplyr::mutate("Olfactometer Zone" = E) %>%
      dplyr::mutate(Treatment = sum(G)) %>%
      dplyr::mutate("Zone Assignment" = "Treatment")

    tbl_one <- treatment_zone %>%
      dplyr::select("Olfactometer Zone", "Zone Assignment")

    control_zones <- zones %>%
      dplyr::filter(control == TRUE & arms == TRUE) %>%
      dplyr::mutate("Olfactometer Zone" = E) %>%
      dplyr::mutate(Treatment = sum(G)) %>%
      dplyr::mutate("Zone Assignment" = "Control")

    tbl_two <- control_zones %>%
      dplyr::select("Olfactometer Zone", "Zone Assignment")

    results_tbl <- dplyr::bind_rows(tbl_zero, tbl_one, tbl_two) %>%
      dplyr::distinct()

    ordered_zones <- dplyr::arrange(results_tbl, results_tbl$`Olfactometer Zone`)

    results_table <- dplyr::bind_cols(sum_zone_times, times_entered, ordered_zones)

    results_table <- results_table %>%
      dplyr::select(E, time_secs, time_mins, times_entered, `Zone Assignment`) %>%
      dplyr::rename("Olfactometer Zone" = E) %>%
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
  }

  else if (no_treatment_arms == 2) {
    treatment_arm_one <- readline("Olfactometer arm containing treatment one (1:7): ")

    treatment_arm_two <- readline("Olfactometer arm containing treatment two (1:7): ")

    start_timer <- readline("Press any key to begin recording data: ")

    while (TRUE) {
      # open infinite while loop
      tictoc::tic() # start timer
      olfactometer_zone <- readline("Olfactometer zone: ") # allow for entry of state
      if (olfactometer_zone %in% 1:7) { # check if it's acceptable
        elapsed <- tictoc::toc() # if it is then end timer and record data
        utils::write.table(
          cbind(
            experiment,
            replicate,
            central_zone,
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
      } else if (olfactometer_zone == "t") { # if input is 't'
        break # break out of while loop
      } else if (olfactometer_zone < 1 |
        olfactometer_zone > 7 &
          olfactometer_zone != "t") { # if input is not and accepted state AND is not 't'
        print("That is not a valid zone, please use numerical keys 1:7 to select valid zones")
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
      col_names = c("A", "B", "C", "D", "E", "G", "H"),
      col_types = readr::cols("G" = readr::col_integer())
    )

    data <- data %>%
      tidyr::complete(tidyr::nesting(A, B, C, D, E), G = seq(1, 7, 1L)) %>%
      dplyr::arrange(is.na(H)) %>%
      dplyr::mutate(H = tidyr::replace_na(H, 0))

    times_entered <- data %>%
      tidyr::complete(tidyr::nesting(A, B, C, D, E), G = seq(1, 7, 1L)) %>%
      dplyr::arrange(is.na(H)) %>%
      dplyr::mutate(H = tidyr::replace_na(H, 0)) %>%
      dplyr::add_count(G) %>%
      dplyr::mutate(n = (H != 0) * n) %>%
      dplyr::group_by(G) %>%
      dplyr::summarise_at(dplyr::vars(n), .funs = mean) %>%
      dplyr::pull(n)

    times_entered <- tibble::tibble(times_entered)

    zones <- data %>%
      dplyr::mutate(treatment = G %in% c(D, E)) %>%
      dplyr::mutate(arms = G != C)

    by_zone <- dplyr::group_by(zones, G)

    sum_zone_times <- dplyr::summarise(by_zone, time_secs = sum(H)) %>%
      dplyr::mutate(time_mins = time_secs / 60)

    centre_zone <- zones %>%
      dplyr::filter(arms == FALSE) %>%
      dplyr::mutate("Olfactometer Zone" = G) %>%
      dplyr::mutate(Centre = sum(H)) %>%
      dplyr::mutate("Zone Assignment" = "Centre")

    tbl_zero <- centre_zone %>%
      dplyr::select("Olfactometer Zone", "Zone Assignment")

    treatment_zone <- zones %>%
      dplyr::filter(treatment == TRUE) %>%
      dplyr::mutate("Olfactometer Zone" = G) %>%
      dplyr::mutate(Treatment = sum(H)) %>%
      dplyr::mutate("Zone Assignment" = "Treatment")

    tbl_one <- treatment_zone %>%
      dplyr::select("Olfactometer Zone", "Zone Assignment")

    control_zones <- zones %>%
      dplyr::filter(treatment == FALSE & arms == TRUE) %>%
      dplyr::mutate("Olfactometer Zone" = G) %>%
      dplyr::mutate(Treatment = sum(H)) %>%
      dplyr::mutate("Zone Assignment" = "Control")

    tbl_two <- control_zones %>%
      dplyr::select("Olfactometer Zone", "Zone Assignment")

    results_tbl <- dplyr::bind_rows(tbl_zero, tbl_one, tbl_two) %>%
      dplyr::distinct()

    ordered_zones <- dplyr::arrange(results_tbl, results_tbl$`Olfactometer Zone`)

    results_table <- dplyr::bind_cols(sum_zone_times, times_entered, ordered_zones)

    results_table <- results_table %>%
      dplyr::select(G, time_secs, time_mins, times_entered, "Zone Assignment") %>%
      dplyr::rename("Olfactometer Zone" = G) %>%
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
  }
}
