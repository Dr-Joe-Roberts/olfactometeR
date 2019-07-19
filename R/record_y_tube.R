#' Record choice data from Y-tube olfactometer experiments
#' @name record_y_tube
#'
#' @description \code{record_y_tube} allows the user to record the choices made by
#' study subjects in Y-tube olfactometer experiments.
#'
#' Upon executing \code{record_y_tube} the user will be prompted in the console to enter the
#' following information before recording: User initials', 'Year of experiment',
#' 'Experiment number', 'Replicate number' and 'Olfactometer arm containing treatment'.
#'
#' Each Y-tube olfactometer arm corresponds to a numerical key, either one or two. When
#' a study subject enters an olfactometer arm and crosses the pre-determined
#' decision line the user must use the numerical key corresponding to the olfactometer arm to
#' record the individual as making a choice. Recording will automatically terminate once
#' a choice has been made.
#'
#' @usage record_y_tube()
#'
#' @return code{record_y_tube} returns a summary table in the console.
#'
#' @examples
#' \dontrun{
#' library(olfactometeR)
#' record_y_tube()
#' User initials: JR
#' Year: 2019
#' Study species being testsed: "Phytoseiulus persimilis"
#' Experiment number: 1
#' Replicate number: 1
#' Olfactometer arm containing treatment (1/2): 2
#' Treatment: "Linalool"
#'
#'
#'  |Variable                 |User Response             |
#'  |:------------------------|:-------------------------|
#'  |User initials            |JR                        |
#'  |Study year               |2030                      |
#'  |Study subject species    |"Phytoseiulus persimilis" |
#'  |Experiment no.           |1                         |
#'  |Replicate no.            |1                         |
#'  |Treatment arm assignment |2                         |
#'  |Treatment                |"linalool"                |
#'
#'  Are the entered details correct (y/n): y
#'  Press any key to begin recording data:
#'  Olfactometer zone: 1
#'  29.2 sec elapsed
#'
#'
#'  |Olfactometer Arm|Zone Assignment|Time to Reach Arm End (secs)|Time to Reach Arm End (mins)|
#'  |:--------------:|:-------------:|:--------------------------:|:--------------------------:|
#'  |       1        |    Control    |           29.2             |           0.49             |
#'  |       2        |   Treatment   |            NA              |            NA              |
#'  Save the ouput (y/n): n
#'  [1] "Output has not been saved"
#' }
#'
#' @export
#'
record_y_tube <- function() {
  user <- readline("User initials: ")

  year <- readline("Year: ")

  species <- readline("Study species being tested: ")

  experiment <- readline("Experiment number: ")

  replicate <- readline("Replicate number: ")

  treatment_arm <-
    readline("Olfactometer arm containing treatment (1/2): ")

  treatment_ID <- readline("Treatment: ")

  variable_table <- tibble::tibble(
    "User initials" = user,
    "Study year" = year,
    "Study subject species" = species,
    "Experiment no." = experiment,
    "Replicate no." = replicate,
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
    start_timer <- readline("Press any key to begin recording data: ")

    while (TRUE) {
      tictoc::tic()
      olfactometer_zone <-
        readline("Olfactometer zone: ")
      if (olfactometer_zone %in% 1:2) {
        elapsed <- tictoc::toc()
        utils::write.table(
          cbind(
            experiment,
            replicate,
            species,
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
            "Y_Tube_Olfactometer_Recording.txt",
            sep = "_"
          ),
          col.names = FALSE,
          row.names = FALSE,
          quote = FALSE,
          append = TRUE
        )
        break
      } else if (olfactometer_zone < 1 |
        olfactometer_zone > 2) {
        print("That is not a valid zone, please use numerical keys 1:2 to select valid zones")
      }
    }

    data <- readr::read_delim(
      paste(
        user,
        year,
        experiment,
        replicate,
        "Y_Tube_Olfactometer_Recording.txt",
        sep = "_"
      ),
      delim = " ",
      col_names = c("A", "B", "C", "D", "E", "G", "H"),
      col_types = readr::cols("G" = readr::col_integer())
    )

    data <- data %>%
      tidyr::complete(tidyr::nesting(A, B, C, D, E), G = seq(1, 2, 1L)) %>%
      dplyr::arrange(is.na(H)) %>%
      dplyr::arrange(G)

    arms <- data %>%
      dplyr::mutate(control = G != D)

    by_arms <- dplyr::group_by(arms, G)

    sum_zone_times <- dplyr::summarise(by_arms, time_secs = sum(H)) %>%
      dplyr::mutate(time_mins = time_secs / 60)

    treatment_zone <- arms %>%
      dplyr::filter(control == FALSE) %>%
      dplyr::mutate("Olfactometer Zone" = G) %>%
      dplyr::mutate(Treatment = sum(H)) %>%
      dplyr::mutate("Zone Assignment" = "Treatment")

    tbl_one <- treatment_zone %>%
      dplyr::select("Olfactometer Zone", "Zone Assignment")

    control_zones <- arms %>%
      dplyr::filter(control == TRUE) %>%
      dplyr::mutate("Olfactometer Zone" = G) %>%
      dplyr::mutate(Treatment = sum(H)) %>%
      dplyr::mutate("Zone Assignment" = "Control")

    tbl_two <- control_zones %>%
      dplyr::select("Olfactometer Zone", "Zone Assignment")

    results_tbl <- dplyr::bind_rows(tbl_one, tbl_two) %>%
      dplyr::distinct()

    ordered_arms <- dplyr::arrange(results_tbl, results_tbl$`Olfactometer Zone`)

    results_table <- dplyr::bind_cols(sum_zone_times, ordered_arms)

    results_table <- results_table %>%
      dplyr::select(G, "Zone Assignment", time_secs, time_mins) %>%
      dplyr::rename("Olfactometer Arm" = G) %>%
      dplyr::rename("Time to Arm End (secs)" = time_secs) %>%
      dplyr::rename("Time to Arm End (mins)" = time_mins)

    species_ID <- data %>%
      dplyr::ungroup(B) %>%
      dplyr::select(C) %>%
      dplyr::distinct()

    treatment_ID <- data %>%
      dplyr::ungroup(B) %>%
      dplyr::select(E) %>%
      dplyr::distinct()

    tbl_hux <- huxtable::as_hux(results_table, add_colnames = TRUE) %>%
      huxtable::theme_article(header_col = FALSE) %>%
      huxtable::set_caption(paste("Y-Tube olfactometer")) %>%
      huxtable::set_caption_pos("topcenter") %>%
      huxtable::set_align("centre") %>%
      huxtable::set_bold(1, 1:4, FALSE) %>%
      huxtable::add_footnote(paste("Study species:", species_ID)) %>%
      huxtable::add_footnote(paste("Treatment:", treatment_ID), border = 0)

    huxtable::number_format(tbl_hux)[-1, 3:4] <- 2

    huxtable::print_screen(tbl_hux, colnames = FALSE)

    file_export <- readline("Save the ouput (y/n): ")

    if (file_export == "y") {
      huxtable::quick_xlsx(
        tbl_hux,
        file = "Four_Arm_Olfactometer_Recording.xlsx",
        borders = 0.4,
        open = interactive()
      )
    } else if (file_export == "n") {
      base::print("Output has not been saved")
    }
  }

  else if (user_check == "n") {
    print("Variables are incorrect, please execute the function again to re-enter them")
  }
}
