#' resetTimer
#'
#' Simulates a button press for `Reset`.
#'
#' @param inputId character: input slot
#'
#' @return nothing
#' @importFrom shinyjs runjs
#' @export
#'
#' @examples
#' if (interactive()) vignette("shinyDTC")
resetTimer <- function(inputId) {
  runjs(sprintf("$('#%s').click();", paste0(inputId, '_reset')))
}
