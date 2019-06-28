#' Preview and export recorded olfactometer data
#'
#' Uses the console to display a summary of the data collected from one
#' experiment replicate using the record_data function and creates an .xlsx
#' file title "Summary Results  Table". Please make sure you change the name of
#' the exported file if you wish to keep it as the next time this function is
#' executed it will be overwritten.
#'
#' @param x Leave blank
#'
#' @return
#'
#' @examples
#' \dontrun{
#'
#' results_table()
#'
#'
#'| Olfactometer Zone | Total Time in Zone (secs) | Total Time in Zone (mins) | No. of Times Zone Entered | Treatment Arm |
#'|:-----------------:|:-------------------------:|:-------------------------:|:-------------------------:|:-------------:|
#'|         1         |           8.68            |           0.14            |             2             |               |
#'|         2         |          167.89           |           2.80            |             2             |       T       |
#'|         3         |           12.02           |           0.20            |             1             |               |
#'|         4         |           1.78            |           0.03            |             1             |               |
#'|         5         |           3.55            |           0.06            |             1             |               |
#' }
#'
#' @export
#'
results_table = function(x) {
  data = readr::read_delim(
    file.choose(),
    delim = " ",
    col_names = c("A", "B", "C", "D", "E"),
    col_types = readr::cols("D" = readr::col_integer())
  )

  data = data %>%
    tidyr::complete(tidyr::nesting(A, B, C), D = seq(min(D), max(D), 1L)) %>%
    dplyr::arrange(is.na(E)) %>%
    dplyr::mutate(E = tidyr::replace_na(E, 0))

  times_entered = data %>%
    tidyr::complete(tidyr::nesting(A, B, C), D = seq(min(D), max(D), 1L)) %>%
    dplyr::arrange(is.na(E)) %>%
    dplyr::mutate(E = tidyr::replace_na(E, 0)) %>%
    dplyr::add_count(D) %>%
    dplyr::mutate(n = (E != 0) * n) %>%
    dplyr::group_by(D) %>%
    dplyr::summarise_at(dplyr::vars(n), .funs = mean) %>%
    dplyr::pull(n)

  times_entered = tibble::tibble(times_entered)

  zones = data %>%
    dplyr::mutate(control = D != C)

  by_zone = dplyr::group_by(zones, D)

  sum_zone_times = dplyr::summarise(by_zone, time_secs = sum(E)) %>%
    dplyr::mutate(time_mins = time_secs / 60)

  treatment_zone = zones %>%
    dplyr::filter(control == F) %>%
    dplyr::mutate("Olfactometer Zone" = D) %>%
    dplyr::mutate(Treatment = sum(E)) %>%
    dplyr::mutate("Treatment Arm" = "T")

  tbl_one = treatment_zone %>%
    dplyr::select("Olfactometer Zone", "Treatment Arm")

  control_zones = zones %>%
    dplyr::filter(control == T) %>%
    dplyr::mutate("Olfactometer Zone" = D) %>%
    dplyr::mutate(Treatment = sum(E)) %>%
    dplyr::mutate("Treatment Arm" = " ")

  tbl_two = control_zones %>%
    dplyr::select("Olfactometer Zone", "Treatment Arm")

  results_tbl = dplyr::bind_rows(tbl_one, tbl_two) %>%
    dplyr::distinct()

  ordered_zones = dplyr::arrange(results_tbl, results_tbl$`Olfactometer Zone`)

  results_table = dplyr::bind_cols(sum_zone_times, times_entered, ordered_zones)

  results_table = results_table %>%
    dplyr::select(D, time_secs, time_mins, times_entered, `Treatment Arm`) %>%
    dplyr::rename(!!"Olfactometer Zone" := D) %>%
    dplyr::rename(!!"Total Time in Zone (secs)" := time_secs) %>%
    dplyr::rename(!!"Total Time in Zone (mins)" := time_mins) %>%
    dplyr::rename(!!"No. of Times Zone Entered" := times_entered)

  final_table = knitr::kable(utils::head(results_table), format = "markdown", digits = 2, align = "c")

  base::print(final_table)

  rio::export(results_table, "Summary Results Table.xlsx")
}
