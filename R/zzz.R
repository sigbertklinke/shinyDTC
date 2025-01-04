timerenv <- new.env()

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0('shinyDTC ', utils::packageVersion("shinyDTC"),
                               ': see the package vignette with `vignette("shinyDTC")`'))
}
