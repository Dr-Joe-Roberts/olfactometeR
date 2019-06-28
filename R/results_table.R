#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
results_table = function(x) {
  data = read_delim(
    file.choose(),
    delim = " ",
    col_names = c("A", "B", "C", "D", "E"),
    col_types = cols("D" = col_integer())
  )

  data = data %>%
    complete(nesting(A, B, C), D = seq(min(D), max(D), 1L)) %>%
    arrange(is.na(E)) %>%
    mutate(E = replace_na(E, 0))

  times_entered = data %>%
    complete(nesting(A, B, C), D = seq(min(D), max(D), 1L)) %>%
    arrange(is.na(E)) %>%
    mutate(E = replace_na(E, 0)) %>%
    add_count(D) %>%
    mutate(n = (E != 0) * n) %>%
    group_by(D) %>%
    summarise_at(vars(n), .funs = mean) %>%
    pull(n)

  times_entered = tibble(times_entered)

  zones = data %>%
    mutate(control = D != C)

  by_zone = group_by(zones, D)

  sum_zone_times = summarise(by_zone, time_secs = sum(E)) %>%
    mutate(time_mins = time_secs / 60)

  treatment_zone = zones %>%
    filter(control == F) %>%
    mutate("Olfactometer Zone" = D) %>%
    mutate(Treatment = sum(E)) %>%
    mutate("Treatment Arm" = "T")

  tbl_one = treatment_zone %>%
    select("Olfactometer Zone", "Treatment Arm")

  control_zones = zones %>%
    filter(control == T) %>%
    mutate("Olfactometer Zone" = D) %>%
    mutate(Treatment = sum(E)) %>%
    mutate("Treatment Arm" = " ")

  tbl_two = control_zones %>%
    select("Olfactometer Zone", "Treatment Arm")

  results_tbl = bind_rows(tbl_one, tbl_two) %>%
    distinct

  ordered_zones = arrange(results_tbl, results_tbl$`Olfactometer Zone`)

  results_table = bind_cols(sum_zone_times, times_entered, ordered_zones)

  results_table = results_table %>%
    select(D, time_secs, time_mins, times_entered, `Treatment Arm`) %>%
    rename(!!"Olfactometer Zone" := D) %>%
    rename(!!"Total Time in Zone (secs)" := time_secs) %>%
    rename(!!"Total Time in Zone (mins)" := time_mins) %>%
    rename(!!"No. of Times Zone Entered" := times_entered)

  final_table = kable(head(results_table), format = "markdown", digits = 2, align = "c")

  print(final_table)

  export(results_table, "Summary Results Table.xlsx")
}
