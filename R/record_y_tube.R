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
#' Each Y-tube olfactometer arm corresponds to a numerical key, either one or two.
#' When a study subject enters an olfactometer arm and crosses the pre-determined
#' decision line the user must use the numerical key corresponding to the olfactometer arm to
#' record the individual as making a choice. Recording will automatically terminate once
#' a choice has been made.
#'
#' @usage record_y_tube()
#'
#' @return code{record_y_tube} returns a summary table in the console and exports
#' an .xlsx file containing these values to the user's working directory.
#'
#' @examples
#' \dontrun{> library(olfactometeR)
#' record_y_tube()
#' User initials: JR
#' Year: 2019
#' Experiment number: 1
#' Replicate number: 1
#' Olfactometer arm containing treatment: 2
#' Press any key to begin recording data:
#' Olfactometer zone: 1
#' 99.97 sec elapsed


#' |Olfactometer Arm|Time to Reach Arm End (secs)|Time to Reach Arm End (mins)|Treatment Arm|
#' |:--------------:|:--------------------------:|:--------------------------:|:-----------:|
#' |        1       |            99.97           |             1.67           |             |
#' |        2       |              NA            |              NA            |      T      |
#' }
#'
#' @export
#'
record_y_tube <- function() {
  user <- readline("User initials: ")

  year <- readline("Year: ")

  experiment <- readline("Experiment number: ")

  replicate <- readline("Replicate number: ")

  treatment_arm <-
    readline("Olfactometer arm containing treatment: ")

  start_timer <- readline("Press any key to begin recording data: ")

  while (TRUE) {
    # open infinite while loop
    tictoc::tic() # start timer
    olfactometer_zone <-
      readline("Olfactometer zone: ") # allow for entry of state
    if (olfactometer_zone %in% 1:2) {
      # check if it's acceptable
      elapsed <- tictoc::toc() # if it is then end timer and record data
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
          "Y_Tube_Olfactometer_Recording.txt",
          sep = "_"
        ),
        col.names = F,
        row.names = F,
        quote = F,
        append = T
      )
      break
    } else if (olfactometer_zone < 1 |
      olfactometer_zone > 2) {
      # if input is not and accepted state AND is not 't'
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
    col_names = c("A", "B", "C", "D", "E"),
    col_types = readr::cols("D" = readr::col_integer())
  )

  data <- data %>%
    tidyr::complete(tidyr::nesting(A, B, C), D = seq(1, 2, 1L)) %>%
    dplyr::arrange(is.na(E)) %>%
    dplyr::arrange(D)

  arms <- data %>%
    dplyr::mutate(control = D != C)

  by_arms <- dplyr::group_by(arms, D)

  sum_zone_times <- dplyr::summarise(by_arms, time_secs = sum(E)) %>%
    dplyr::mutate(time_mins = time_secs / 60)

  treatment_zone <- arms %>%
    dplyr::filter(control == F) %>%
    dplyr::mutate("Olfactometer Zone" = D) %>%
    dplyr::mutate(Treatment = sum(E)) %>%
    dplyr::mutate("Treatment Arm" = "T")

  tbl_one <- treatment_zone %>%
    dplyr::select("Olfactometer Zone", "Treatment Arm")

  control_zones <- arms %>%
    dplyr::filter(control == T) %>%
    dplyr::mutate("Olfactometer Zone" = D) %>%
    dplyr::mutate(Treatment = sum(E)) %>%
    dplyr::mutate("Treatment Arm" = " ")

  tbl_two <- control_zones %>%
    dplyr::select("Olfactometer Zone", "Treatment Arm")

  results_tbl <- dplyr::bind_rows(tbl_one, tbl_two) %>%
    dplyr::distinct()

  ordered_arms <- dplyr::arrange(results_tbl, results_tbl$`Olfactometer Zone`)

  results_table <- dplyr::bind_cols(sum_zone_times, ordered_arms)

  results_table <- results_table %>%
    dplyr::select(D, time_secs, time_mins, `Treatment Arm`) %>%
    dplyr::rename("Olfactometer Arm" = D) %>%
    dplyr::rename("Time to Reach Arm End (secs)" = time_secs) %>%
    dplyr::rename("Time to Reach Arm End (mins)" = time_mins)

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
      "Y_Tube_Olfactometer_Recording_Summary.xlsx",
      sep = "_"
    )
  )
}
