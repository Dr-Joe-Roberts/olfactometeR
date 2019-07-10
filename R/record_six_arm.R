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
#' Centre zone (1/2/3/4/5/6/7): 7
#' Number of treatment arms (1/2): 1
#' Olfactometer arm containing treatment (1/2/3/4/5/6/7): 1
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

  central_zone <- readline("Centre zone (1/2/3/4/5/6/7): ")

  no_treatment_arms <- readline("Number of treatment arms (1/2): ")

  if (no_treatment_arms == 1) {
    treatment_arm <- readline("Olfactometer arm containing treatment (1/2/3/4/5/6/7): ")

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

    data <- read_delim(
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
      col_types = cols("E" = col_integer())
    )

    data <- data %>%
      complete(nesting(A, B, C, D), E = seq(1, 7, 1L)) %>%
      arrange(is.na(G)) %>%
      mutate(G = replace_na(G, 0))

    times_entered <- data %>%
      complete(nesting(A, B, C, D), E = seq(1, 7, 1L)) %>%
      arrange(is.na(G)) %>%
      mutate(G = replace_na(G, 0)) %>%
      add_count(E) %>%
      mutate(n = (G != 0) * n) %>%
      group_by(E) %>%
      summarise_at(vars(n), .funs = mean) %>%
      pull(n)

    times_entered <- tibble(times_entered)

    zones <- data %>%
      mutate(control = E != D) %>%
      mutate(arms = E != C)

    by_zone <- group_by(zones, E)

    sum_zone_times <- summarise(by_zone, time_secs = sum(G)) %>%
      mutate(time_mins = time_secs / 60)

    centre_zone <- zones %>%
      filter(arms == FALSE) %>%
      mutate("Olfactometer Zone" = E) %>%
      mutate(Centre = sum(G)) %>%
      mutate("Zone Assignment" = "Centre")

    tbl_zero <- centre_zone %>%
      select("Olfactometer Zone", "Zone Assignment")

    treatment_zone <- zones %>%
      filter(control == FALSE) %>%
      mutate("Olfactometer Zone" = E) %>%
      mutate(Treatment = sum(G)) %>%
      mutate("Zone Assignment" = "Treatment")

    tbl_one <- treatment_zone %>%
      select("Olfactometer Zone", "Zone Assignment")

    control_zones <- zones %>%
      filter(control == TRUE & arms == TRUE) %>%
      mutate("Olfactometer Zone" = E) %>%
      mutate(Treatment = sum(G)) %>%
      mutate("Zone Assignment" = "Control")

    tbl_two <- control_zones %>%
      select("Olfactometer Zone", "Zone Assignment")

    results_tbl <- bind_rows(tbl_zero, tbl_one, tbl_two) %>%
      distinct()

    ordered_zones <- arrange(results_tbl, results_tbl$`Olfactometer Zone`)

    results_table <- bind_cols(sum_zone_times, times_entered, ordered_zones)

    results_table <- results_table %>%
      select(E, time_secs, time_mins, times_entered, `Zone Assignment`) %>%
      rename("Olfactometer Zone" = E) %>%
      rename("Total Time in Zone (secs)" = time_secs) %>%
      rename("Total Time in Zone (mins)" = time_mins) %>%
      rename("No. of Times Zone Entered" = times_entered)

    final_table <- kable(
      results_table,
      format = "markdown",
      digits = 2,
      align = "c"
    )

    print(final_table)

    export(
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
    treatment_arm_one <- readline("Olfactometer arm containing treatment one (1/2/3/4/5/6/7): ")

    treatment_arm_two <- readline("Olfactometer arm containing treatment two (1/2/3/4/5/6/7): ")

    start_timer <- readline("Press any key to begin recording data: ")

    while (TRUE) {
      # open infinite while loop
      tic() # start timer
      olfactometer_zone <- readline("Olfactometer zone: ") # allow for entry of state
      if (olfactometer_zone %in% 1:7) { # check if it's acceptable
        elapsed <- toc() # if it is then end timer and record data
        write.table(
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

    data <- read_delim(
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
      col_types = cols("G" = col_integer())
    )

    data <- data %>%
      complete(nesting(A, B, C, D, E), G = seq(1, 7, 1L)) %>%
      arrange(is.na(H)) %>%
      mutate(H = replace_na(H, 0))

    times_entered <- data %>%
      complete(nesting(A, B, C, D, E), G = seq(1, 7, 1L)) %>%
      arrange(is.na(H)) %>%
      mutate(H = replace_na(H, 0)) %>%
      add_count(G) %>%
      mutate(n = (H != 0) * n) %>%
      group_by(G) %>%
      summarise_at(vars(n), .funs = mean) %>%
      pull(n)

    times_entered <- tibble(times_entered)

    zones <- data %>%
      mutate(treatment = G %in% c(D, E)) %>%
      mutate(arms = G != C)

    by_zone <- group_by(zones, G)

    sum_zone_times <- summarise(by_zone, time_secs = sum(H)) %>%
      mutate(time_mins = time_secs / 60)

    centre_zone <- zones %>%
      filter(arms == FALSE) %>%
      mutate("Olfactometer Zone" = G) %>%
      mutate(Centre = sum(H)) %>%
      mutate("Zone Assignment" = "Centre")

    tbl_zero <- centre_zone %>%
      select("Olfactometer Zone", "Zone Assignment")

    treatment_zone <- zones %>%
      filter(treatment == TRUE) %>%
      mutate("Olfactometer Zone" = G) %>%
      mutate(Treatment = sum(H)) %>%
      mutate("Zone Assignment" = "Treatment")

    tbl_one <- treatment_zone %>%
      select("Olfactometer Zone", "Zone Assignment")

    control_zones <- zones %>%
      filter(treatment == FALSE & arms == TRUE) %>%
      mutate("Olfactometer Zone" = G) %>%
      mutate(Treatment = sum(H)) %>%
      mutate("Zone Assignment" = "Control")

    tbl_two <- control_zones %>%
      select("Olfactometer Zone", "Zone Assignment")

    results_tbl <- bind_rows(tbl_zero, tbl_one, tbl_two) %>%
      distinct()

    ordered_zones <- arrange(results_tbl, results_tbl$`Olfactometer Zone`)

    results_table <- bind_cols(sum_zone_times, times_entered, ordered_zones)

    results_table <- results_table %>%
      select(G, time_secs, time_mins, times_entered, "Zone Assignment") %>%
      rename("Olfactometer Zone" = G) %>%
      rename("Total Time in Zone (secs)" = time_secs) %>%
      rename("Total Time in Zone (mins)" = time_mins) %>%
      rename("No. of Times Zone Entered" = times_entered)

    final_table <- kable(
      results_table,
      format = "markdown",
      digits = 2,
      align = "c"
    )

    print(final_table)

    export(
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
