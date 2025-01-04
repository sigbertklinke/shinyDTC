#' updateTimerInput
#'
#' @param session session object
#' @param inputId id of the input object
#' @param step1 modified values for `Step` button, see [shiny::updateActionButton()]
#' @param reset modified values for `Reset` button, see [shiny::updateActionButton()]
#' @param ... modified values for `Speed` slider, see [shiny::updateSliderInput()]
#'
#' @return nothing
#' @importFrom shiny getDefaultReactiveDomain updateSliderInput updateActionButton
#' @export
#'
#' @examples
#' if (interactive()) vignette("shinyDTC")
updateTimerInput <- function(session=getDefaultReactiveDomain(), inputId, step1=list(), reset=list(), ...) {
  args <- list(...)
  if (length(args)) {
    args$session <- session
    args$inputId <- paste0(inputId, "_speed")
    do.call("updateSliderInput", args)
  }
  if (length(step1)) {
    step1$session <- session
    step1$inputId <- paste0(inputId, "_step")
    do.call("updateActionButton", step1)
  }
  if (length(reset)) {
    reset$session <- session
    reset$inputId <- paste0(inputId, "_reset")
    do.call("updateActionButton", reset)
  }
}
