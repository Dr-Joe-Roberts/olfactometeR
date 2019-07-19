#' Create a unique ID for naming files
#' @name make_unique_id
#'
#' @description \code{make_unique_id} is used purely to generate unique file names for exported tables.
#'
#' @param safe Logical value - do not edit
#'
#' @return code{make_unique_id} returns a unique ID name
#'
#' @examples
#' \dontrun{
#' make_unique_id()
#' [1] "5d31f2bb"
#' }
#'
#' @export
#'
make_unique_id <- function(safe = TRUE) {
  uniqid <- function() base::as.hexmode(base::as.integer(base::Sys.time() + stats::runif(1) * 1000))

  if (!safe) return(uniqid())

  callr::r_safe(
    function() olfactometeR::make_unique_id(safe = FALSE)
  )
}
