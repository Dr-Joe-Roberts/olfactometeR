#' \code{record_four_arm} - Records data from four-arm olfactometers
#' @name record_four_arm
#'
#' @param x Leave blank
#'
#' @return \code{record_four_arm} allows the user to interactively record an
#' individual's movements in a four-arm olfactometer, returing a summary
#' table in the console and exporting an .xlsx file containing the results.
#'
#'
#' @examples
#' \dontrun{record_four_arm()
#'
#' User initials: JR
#' Year: 2019
#' Experiment number: 1
#' Replicate number: 1
#' Olfactometer arm containing treatment: 2
#' Press any key to begin recording data: s
#' Olfactometer zone: 5
# '16.3 sec elapsed
#' Olfactometer zone: 4
#' 13.7 sec elapsed
#' Olfactometer zone: 2
#' 6.19 sec elapsed
#' Olfactometer zone: 5
#' 6.61 sec elapsed
#' Olfactometer zone: 2
#' 45.67 sec elapsed
#' Olfactometer zone: t
#' }
#'
#' @export
#'
record_four_arm <- function(x) {

  user <- readline("User initials: ")

  year <- readline("Year: ")

  experiment <- readline("Experiment number: ")

  replicate <- readline("Replicate number: ")

  treatment_arm <- readline("Olfactometer arm containing treatment: ")

  start_timer <- readline("Press any key to begin recording data: ")


  while (TRUE) {
    tictoc::tic() # start timer
    olfactometer_zone <- readline("Olfactometer zone: ") # allow for entry of state
    if (olfactometer_zone %in% 1:5) { # check if it's acceptable
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
          "Olfactometer_Recording.txt",
          sep = "_"
        ),
        col.names = F,
        row.names = F,
        quote = F,
        append = T
      )
    } else if (olfactometer_zone == "t") { # if input is 't'
      break # break out of while loop
    } else if (olfactometer_zone < 1 |
      olfactometer_zone > 5 &
        olfactometer_zone != "t") { # if input is not and accepted state AND is not 't'
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
    dplyr::filter(control == F) %>%
    dplyr::mutate("Olfactometer Zone" = D) %>%
    dplyr::mutate(Treatment = sum(E)) %>%
    dplyr::mutate("Treatment Arm" = "T")

  tbl_one <- treatment_zone %>%
    dplyr::select("Olfactometer Zone", "Treatment Arm")

  control_zones <- zones %>%
    dplyr::filter(control == T) %>%
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
    dplyr::rename(!!"Olfactometer Zone" == D) %>%
    dplyr::rename(!!"Total Time in Zone (secs)" == time_secs) %>%
    dplyr::rename(!!"Total Time in Zone (mins)" == time_mins) %>%
    dplyr::rename(!!"No. of Times Zone Entered" == times_entered)

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
