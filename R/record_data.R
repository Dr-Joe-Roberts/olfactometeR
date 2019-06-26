#' Record four-arm olfactometer data
#'
#' Create a .txt file containing the duration of time a study subject spends
#' in each of the five olfactometer zones and the number of times each zone
#' was entered. When this function is called the user will be prompted in the
#' console to enter experimental details required for naming the .txt file and
#' for further analysis.
#'
#' @param
#'
#' @return
#' @export .txt file
#'
#' @examples
#' record_data()
record_data = function(x) {
  suppressPackageStartupMessages({
    require(tictoc) #load required package
  })

  user = readline("User initials: ")

  year = readline("Year: ")

  experiment = readline("Experiment number: ")

  replicate = readline("Replicate number: ")

  treatment_arm = readline("Olfactometer arm containing treatment: ")

  start_timer = readline("Press s to begin recording data: ")


  while (T) {
    #open infinite while loop
    tic()           #start timer
    olfactometer_zone = readline("Olfactometer zone: ")  #allow for entry of state
    if (olfactometer_zone %in% 1:5) {
      #check if it's acceptable
      elapsed = toc()            #if it is then end timer and record data
      write.table(
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
