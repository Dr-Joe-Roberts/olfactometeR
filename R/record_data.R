#' Record four-arm olfactometer data
#'
#' Create a .txt file containing the duration of time a study subject spends
#' in each of the five olfactometer zones and the number of times each zone
#' was entered. When this function is called the user will be prompted in the
#' console to enter experimental details required for naming the .txt file and
#' for further analysis.
#'
#' @param x Leave blank
#'
#' @return
#'
#' @examples
#' \dontrun{
#' record_data()
#'
#' User initials: JR
#' Year: 2019
#' Experiment number: 1
#' Replicate number: 1
#' Olfactometer arm containing treatment: 2
#' Press s to begin recording data: s
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
#'
#' }
#' @export

record_data = function(x) {

  user = readline("User initials: ")

  year = readline("Year: ")

  experiment = readline("Experiment number: ")

  replicate = readline("Replicate number: ")

  treatment_arm = readline("Olfactometer arm containing treatment: ")

  start_timer = readline("Press s to begin recording data: ")

  while (T) {
    #open infinite while loop
    tictoc::tic()           #start timer
    olfactometer_zone = readline("Olfactometer zone: ")  #allow for entry of state
    if (olfactometer_zone %in% 1:5) {
      #check if it's acceptable
      elapsed = tictoc::toc()            #if it is then end timer and record data
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
    } else if (olfactometer_zone == 't') {
      #if input is 't'
      break                    #break out of while loop
    } else if (olfactometer_zone < 1 |
               olfactometer_zone > 5 &
               olfactometer_zone != 't') {
      #if input is not and accepted state AND is not 't'
      print('That is not a valid zone, please use numerical keys 1:5 to select valid zones')
    }
  }
}
