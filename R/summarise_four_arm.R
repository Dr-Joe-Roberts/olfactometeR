#' Summarise four-arm olfactometer experiment results
#' @name summarise_four_arm
#'
#' @description \code{summarise_four_arm} allows the user to summarise replicates
#' from four-arm olfactometer experiments.
#'
#' Upon executing \code{summarise_four_arm} the user will be prompted to select the
#' experiment replicates they want to summarise. displaying a summary table in the
#' console that can be exported as a .xlsx file if required.
#'
#' @usage summarise_four_arm()
#'
#' @return \code{summarise_four_arm} returns a summary table of the experiment
#' replicates to the console, which can be exported as a .xlsx file if required.
#'
#' @examples
#' \dontrun{
#' library(olfactometeR)
#' summarise_four_arm()
#' Number of treatment arms (1/2): 1
#'
#'
#' | Replicate | Centre | Treatment | Control 1 | Control 2 | Control 3 | Control mean |
#' |:---------:|:------:|:---------:|:---------:|:---------:|:---------:|:------------:|
#' |     1     | 72.52  |   4.83    |   0.00    |   90.47   |   4.20    |    31.56     |
#' |     2     | 20.06  |   3.98    |   0.00    |   21.71   |  366.61   |    129.44    |
#' |     3     | 62.03  |   1.32    |  129.27   |   79.26   |   0.00    |    69.51     |
#'
#' Save the ouput as an .xlsx file? (y/n) n
#'
#'
#' |                          |
#' |:-------------------------|
#' |Output has not been saved |
#' }
#'
#' @export
#'
summarise_four_arm <- function() {

  treatment_arm_no <- readline("Number of treatment arms (1/2): ")

  if (treatment_arm_no == 1) {
    files <- tcltk::tk_choose.files(
      default = "*.txt",
      caption = "Select files",
      multi = TRUE,
      filters = NULL,
      index = 1
    )

    tbl <- files %>%
      purrr::map_df(~ readr::read_delim(
        .,
        delim = " ",
        col_names = c("A", "B", "C", "D", "E", "G", "H", "I"),
        col_types = readr::cols("H" = readr::col_integer())
      ))

    data <- tbl %>%
      tidyr::complete(tidyr::nesting(A, B, C, D, E, G), H = seq(1, 5, 1L)) %>%
      dplyr::arrange(is.na(I)) %>%
      dplyr::mutate(I = tidyr::replace_na(I, 0))

    zones <- data %>%
      dplyr::mutate(control = H != E) %>%
      dplyr::mutate(arms = H != D) %>%
      dplyr::arrange(B)

    centre_zone <- zones %>%
      dplyr::filter(arms == FALSE) %>%
      dplyr::group_by(B) %>%
      dplyr::summarise("Time" = sum(I)) %>%
      dplyr::mutate("Zone" = "Centre") %>%
      dplyr::rename("Replicate" = B)

    tbl_zero <- centre_zone %>%
      dplyr::select("Replicate", "Zone", "Time")

    treatment_zone <- zones %>%
      dplyr::filter(control == FALSE & arms == TRUE) %>%
      dplyr::group_by(B) %>%
      dplyr::summarise("Time" = sum(I)) %>%
      dplyr::mutate("Zone" = "Treatment") %>%
      dplyr::rename("Replicate" = B)

    tbl_one <- treatment_zone %>%
      dplyr::select("Replicate", "Zone", "Time")

    control_zones <- zones %>%
      dplyr::filter(control == TRUE & arms == TRUE) %>%
      dplyr::group_by(B, H) %>%
      dplyr::summarise("Time" = sum(I)) %>%
      dplyr::mutate("Control", 1:3) %>%
      dplyr::mutate("Zone" = paste0("Control", "_", 1:3)) %>%
      dplyr::rename("Replicate" = B)

    tbl_two <- control_zones %>%
      dplyr::select("Replicate", "Zone", "Time")

    control_mean <- control_zones %>%
      dplyr::group_by(Replicate) %>%
      dplyr::mutate("Control mean" = mean(Time))

    tbl_three <- control_mean %>%
      dplyr::select("Replicate", "Control mean") %>%
      dplyr::distinct()

    tbl_four <- dplyr::bind_rows(tbl_zero, tbl_one, tbl_two)

    tbl_five <- tbl_four %>%
      tidyr::spread("Zone", "Time")

    tbl_six <- dplyr::bind_cols(tbl_five, tbl_three) %>%
      dplyr::select("Replicate", "Centre", "Treatment", "Control_1", "Control_2", "Control_3", "Control mean") %>%
      dplyr::rename("Control 1" = "Control_1", "Control 2" = "Control_2", "Control 3" = "Control_3")

    results_tbl <- knitr::kable(
      tbl_six,
      format = "markdown",
      digits = 2,
      align = "c"
    )

    base::print(results_tbl)

    file_export <- readline("Save the ouput as an .xlsx file? (y/n) ")

    if (file_export == "y") {
      rio::export(
        results_table,
        paste(
          user,
          year,
          experiment,
          replicate,
          "Experiment_Summary.xlsx",
          sep = "_"
        )
      )
    } else if (file_export == "n") {
      output_no <- knitr::kable("Output has not been saved", col.names = " ")

      base::print(output_no)
    }
  }

  if (treatment_arm_no == 2) {
    files <- tcltk::tk_choose.files(
      default = "*.txt",
      caption = "Select files",
      multi = TRUE,
      filters = NULL,
      index = 1
    )

    tbl <- files %>%
      purrr::map_df(~ readr::read_delim(
        .,
        delim = " ",
        col_names = c("A", "B", "C", "D", "E", "G", "H", "I", "J", "K"),
        col_types = readr::cols("J" = readr::col_integer())
      ))

    data <- tbl %>%
      tidyr::complete(tidyr::nesting(A, B, C, D, E, G, H, I), J = seq(1, 5, 1L)) %>%
      dplyr::arrange(is.na(K)) %>%
      dplyr::mutate(K = tidyr::replace_na(K, 0))

    zones <- data %>%
      dplyr::group_by(B) %>%
      dplyr::mutate(treatment = J %in% c(E, H)) %>%
      dplyr::mutate(treatment_one = J == E) %>%
      dplyr::mutate(treatment_two = J == H) %>%
      dplyr::mutate(arms = J != D) %>%
      dplyr::arrange(B)

    centre_zone <- zones %>%
      dplyr::filter(arms == FALSE) %>%
      dplyr::group_by(B) %>%
      dplyr::mutate("Time" = sum(K)) %>%
      dplyr::mutate("Zone" = "Centre") %>%
      dplyr::rename("Replicate" = B)

    tbl_zero <- centre_zone %>%
      dplyr::select("Replicate", "Zone", "Time")

    treatment_one <- zones %>%
      dplyr::filter(treatment_one == TRUE) %>%
      dplyr::group_by(B) %>%
      dplyr::summarise("Time" = sum(K)) %>%
      dplyr::mutate("Zone" = "Treatment 1") %>%
      dplyr::rename("Replicate" = B)

    tbl_one <- treatment_one %>%
      dplyr::select("Replicate", "Zone", "Time")

    treatment_two <- zones %>%
      dplyr::filter(treatment_two == TRUE) %>%
      dplyr::group_by(B) %>%
      dplyr::summarise("Time" = sum(K)) %>%
      dplyr::mutate("Zone" = "Treatment 2") %>%
      dplyr::rename("Replicate" = B)

    tbl_two <- treatment_two %>%
      dplyr::select("Replicate", "Zone", "Time")

    control_zones <- zones %>%
      dplyr::filter(treatment == FALSE & arms == TRUE) %>%
      dplyr::group_by(B, J) %>%
      dplyr::summarise("Time" = sum(K)) %>%
      dplyr::mutate("Control", 1:2) %>%
      dplyr::mutate("Zone" = paste0("Control", "_", 1:2)) %>%
      dplyr::rename("Replicate" = B)

    tbl_three <- control_zones %>%
      dplyr::select("Replicate", "Zone", "Time")

    control_mean <- control_zones %>%
      dplyr::group_by(Replicate) %>%
      dplyr::mutate("Control mean" = mean(Time))

    tbl_four <- control_mean %>%
      dplyr::select("Replicate", "Control mean") %>%
      dplyr::distinct()

    tbl_five <- dplyr::bind_rows(tbl_zero, tbl_one, tbl_two, tbl_three) %>%
      dplyr::distinct()

    tbl_six <- tbl_five %>%
      tidyr::spread("Zone", "Time")

    tbl_seven <- dplyr::bind_cols(tbl_six, tbl_four) %>%
      dplyr::select("Replicate", "Centre", "Treatment 1", "Treatment 2", "Control_1", "Control_2", "Control mean") %>%
      dplyr::rename("Control 1" = "Control_1", "Control 2" = "Control_2")

    results_tbl <- knitr::kable(
      tbl_seven,
      format = "markdown",
      digits = 2,
      align = "c"
    )

    base::print(results_tbl)

    file_export <- readline("Save the ouput as an .xlsx file? (y/n) ")

    if (file_export == "y") {
      rio::export(
        results_table,
        paste(
          user,
          year,
          experiment,
          replicate,
          "Experiment_Summary.xlsx",
          sep = "_"
        )
      )
    } else if (file_export == "n") {
      output_no <- knitr::kable("Output has not been saved", col.names = " ")

      base::print(output_no)
    }
  }
}
