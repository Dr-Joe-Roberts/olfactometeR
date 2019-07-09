#' Records data from Y-tube olfactometers
#' @name record_y_tube
#'
#' @param x Leave blank
#'
#' @return code{record_y_tube} allows the user to interactively record the
#' choice of an individual in a Y-tubeolfactometer, returing a summary
#' table in the console and exporting an .xlsx file containing the result.
#'
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
record_y_tube <- function(x) {
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
