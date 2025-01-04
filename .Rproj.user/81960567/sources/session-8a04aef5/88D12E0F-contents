#' openApp
#'
#' Opens an example Shiny app into the RStudio editor.
#'
#' @param dir character: name of the app
#' @param ... further parameter given to [rstudioapi::navigateToFile()]
#'
#' @return invisibly the result of `navigateToFile`
#' @importFrom rstudioapi navigateToFile
#' @export
#'
#' @examples
#' if (interactive()) run() # runs the minimal example
openApp <- function(dir="mini", ...) {
  appdir <- system.file('examples-shiny', package="shinyDTC")
  appdir <- list.dirs(appdir, recursive=FALSE)
  i <- pmatch(dir, basename(appdir), nomatch=0L)
  if (i==0L) stop(sprintf("'dir' should be one of %s", paste(basename(appdir), collapse = ", ")))
  invisible(navigateToFile(paste0(appdir[i], '/app.R')))
}
