#' runAppx
#'
#' Runs an example Shiny app via [shiny::runExample()].
#'
#' @details Note that for running a Shiny app more libraries might be necessary.
#'
#' @param example character: name of the example to run, or NA to list the available examples
#' @param x data frame: data to pass to the app
#' @param ... further parameter given to [shiny::runExample()]
#'
#' @return invisibly the (modified) data set `x`
#' @importFrom shiny runExample
#' @export
#'
#' @examples
#' if (interactive()) runAppx() # runs the minimal example
runAppx <- function(example="mini", x=NULL, ...) {
  tmpfile <- tempfile(pattern="dtc", fileext=".rds")
  saveRDS(x, tmpfile, version=2)
  options(shinyDTC.data=tmpfile)
  args <- list(...)
  args$example <- example
  args$package <- "shinyDTC"
  do.call(runExample, args)
  x <- readRDS(tmpfile)
  invisible(x)
}
