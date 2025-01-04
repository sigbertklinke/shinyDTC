#' stepTimer
#'
#' Simulates a button press on `Step`.
#'
#' @param inputId character: input slot
#'
#' @return nothing
#' @importFrom shinyjs runjs
#' @export
#'
#' @examples
#' if (interactive()) vignette("shinyDTC")
stepTimer <- function(inputId) { runjs(sprintf("$('#%s').click();", paste0(inputId, '_step'))) }
