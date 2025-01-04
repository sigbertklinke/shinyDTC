#' timerInput
#'
#' Constructs a timer widget with a slider and two buttons.
#'
#' @param inputId character: input slot
#' @param step1 list: list of parameters for the left (step) button
#' @param reset list: list of parameters for the right (reset) button
#' @param ... parameters to the slider
#'
#' @return a widget to timer
#' @importFrom shiny div
#' @export
#'
#' @examples
#' if (interactive()) vignette("shinyDTC")
timerInput <- function(inputId, step1=list(), reset=list(), ...) {
  args         <- list(...)
  args$inputId <- paste0(inputId, "_speed")
  args$min     <- args$value <- 0
  if (is.null(args$label)) args$label <- "Speed"
  if (is.null(args$max)) args$max <- 60
  stopifnot("'max' must be positive"=args$max>0)
  step1$inputId <- paste0(inputId, "_step")
  if (is.null(step1$label)) step1$label <- "Step"
  reset$inputId <- paste0(inputId, "_reset")
  if (is.null(reset$label)) reset$label <- "Reset"
  if (!is.null(timerenv[[inputId]])) warning(sprintf("Timer '%s' already exists", inputId))
  timerenv[[inputId]] <- 0
  timerenv[[step1$inputId]] <- 0
  timerenv[[reset$inputId]] <- 0
  timerenv[[paste0(inputId, '_max')]] <- args$max
  div(style=sprintf("display: flex; flex-direction: column; width: %s;", args$width),
      div(style="width: 100%;", do.call("sliderInput", args)),
      # Second row: Buttons (aligned left and right)
      div(style="display: flex; justify-content: space-between; width: 100%;",
          do.call("actionButton", step1), do.call("actionButton", reset))
  )
}
