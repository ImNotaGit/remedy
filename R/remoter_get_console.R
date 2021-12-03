#' Remoter Get Console
#'
#' get from remote server for remoter package, via local console
#'
#' @return invisible(TRUE)
#' @export
remoter_get_console <- function() {
  a <- rstudioapi::getSourceEditorContext()
  s <- a$selection[[1]]
  if (s$text=="") {
    rowi <- s$range$start[1]
    rstudioapi::setSelectionRanges(rstudioapi::document_range(c(rowi,0), c(rowi, Inf)))
    a <- rstudioapi::getSourceEditorContext()
    s <- a$selection[[1]]
    rstudioapi::sendToConsole(sprintf("rg( %s )", s$text), execute=TRUE, echo=TRUE, focus=FALSE)
  } else {
    rstudioapi::sendToConsole(sprintf("rg({\n%s\n})", s$text), execute=TRUE, echo=TRUE, focus=FALSE)
  }
  invisible(TRUE)
}

