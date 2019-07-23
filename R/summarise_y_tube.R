#' Summarise Y-tube olfactometer experiment results
#' @name summarise_y_tube
#'
#' @description \code{summarise_y_tube} allows the user to summarise replicates
#' from Y-tube olfactometer experiments.
#'
#' Upon executing \code{summarise_y_tube} the user will be prompted to select the
#' experiment replicates they want to summarise, displaying a summary table in the
#' console that can be exported as a .xlsx file if required.
#'
#' @usage summarise_y_tube()
#'
#' @return \code{summarise_y_tube} returns a summary table of the experiment
#' replicates to the console, which can be exported as a .xlsx file if required.
#'
#' @examples
#' \dontrun{
#' library(olfactometeR)
#'
#' summarise_y_tube()
#'
#'                            Y-Tube Olfactometer
#' ------------------------------------------------------------------------------
#'      Arm      No. Individuals Choosing Arm   Mean Time to Make Choice (secs)
#' ------------------------------------------------------------------------------
#'   Treatment                2                             128.29
#'    Control                 1                              5.00
#' ------------------------------------------------------------------------------
#'   Study species: Phytoseiulus persimilis
#'   Treatment: Methyl salicylate
#'
#' Save the ouput as an .xlsx file? (y/n) n
#' [1] "Output has not been saved"
#' }
#'
#' @export
#'
summarise_y_tube <- function() {
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
      col_names = c("A", "B", "C", "D", "E", "G", "H"),
      col_types = readr::cols("G" = readr::col_integer())
    ))

  data <- tbl %>%
    tidyr::complete(tidyr::nesting(A, B, C, D, E), G = seq(1, 2, 1L)) %>%
    dplyr::arrange(is.na(H)) %>%
    dplyr::mutate(H = tidyr::replace_na(H, 0)) %>%
    dplyr::arrange(G)

  arms <- data %>%
    dplyr::mutate(control = G != D)

  times_chosen <- arms %>%
    tidyr::complete(tidyr::nesting(A, B, C, D, E), G = seq(1, 2, 1L)) %>%
    dplyr::arrange(is.na(H)) %>%
    dplyr::mutate(H = tidyr::replace_na(H, 0)) %>%
    dplyr::group_by(B) %>%
    dplyr::add_count(G) %>%
    dplyr::mutate(n = (H != 0) * n)

  treatment_arm <- times_chosen %>%
    dplyr::filter(control == FALSE) %>%
    dplyr::group_by(B) %>%
    dplyr::mutate("Olfactometer Arm" = G) %>%
    dplyr::mutate("Time" = sum(H)) %>%
    dplyr::mutate("Arm" = "Treatment")

  tbl_one <- treatment_arm %>%
    dplyr::select(B, "Arm", "Time", n) %>%
    dplyr::rename("Replicate" = B) %>%
    dplyr::ungroup(B) %>%
    dplyr::mutate("Mean Time to Make Choice (secs)" = mean(Time))

  control_arm <- times_chosen %>%
    dplyr::filter(control == TRUE) %>%
    dplyr::group_by(B) %>%
    dplyr::mutate("Olfactometer Arm" = G) %>%
    dplyr::mutate("Time" = sum(H)) %>%
    dplyr::mutate("Arm" = "Control")

  tbl_two <- control_arm %>%
    dplyr::select(B, "Arm", "Time", n) %>%
    dplyr::rename("Replicate" = B) %>%
    dplyr::ungroup(B) %>%
    dplyr::mutate("Mean Time to Make Choice (secs)" = mean(Time))

  tbl_three <- dplyr::bind_rows(tbl_one, tbl_two)

  tbl_four <- tbl_three %>%
    dplyr::group_by(Arm) %>%
    dplyr::mutate("No. Individuals Choosing Arm" = sum(n))

  tbl_five <- tbl_four %>%
    dplyr::select("Arm", "No. Individuals Choosing Arm", "Mean Time to Make Choice (secs)") %>%
    dplyr::distinct()

  species_ID <- arms %>%
    dplyr::ungroup(B) %>%
    dplyr::select(C) %>%
    dplyr::distinct()

  treatment_ID <- arms %>%
    dplyr::ungroup(B) %>%
    dplyr::select(E) %>%
    dplyr::distinct()

  tbl_hux <- huxtable::as_hux(tbl_five, add_colnames = TRUE) %>%
    huxtable::theme_article(header_col = FALSE) %>%
    huxtable::set_caption(paste("Y-Tube Olfactometer")) %>%
    huxtable::set_caption_pos("topcenter") %>%
    huxtable::set_align("centre") %>%
    huxtable::set_bold(1, 1:3, FALSE) %>%
    huxtable::add_footnote(paste("Study species:", species_ID)) %>%
    huxtable::add_footnote(paste("Treatment:", treatment_ID), border = 0)

  huxtable::number_format(tbl_hux)[-1, -2] <- 2

  huxtable::print_screen(tbl_hux, colnames = FALSE)

  file_export <- readline("Save the ouput as an .xlsx file? (y/n) ")

  if (file_export == "y") {
    file_path_name <- base::basename(tools::file_path_sans_ext(files))

    file_path_components <- readr::read_delim(file_path_name, delim = "_", col_names = c("A", "B", "C", "D", "E", "G", "H", "I")) %>%
      dplyr::select(A, B, C) %>%
      dplyr::distinct()

    user <- file_path_components %>%
      dplyr::select(A) %>%
      base::as.character()

    year <- file_path_components %>%
      dplyr::select(B) %>%
      base::as.character()

    experiment_no <- file_path_components %>%
      dplyr::select(C)

    huxtable::quick_xlsx(
      tbl_hux,
      file = paste(user, year, "Y_Tube_Experiment", experiment_no, "Summary.xlsx", sep = "_"),
      borders = 0.4,
      open = interactive()
    )
  } else if (file_export == "n") {
    base::print("Output has not been saved")
  }
}
