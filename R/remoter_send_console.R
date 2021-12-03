#' Remoter Send Console
#'
#' send to remote server for remoter package, via local console
#'
#' @return invisible(TRUE)
#' @export
remoter_send_console <- function() {
  a <- rstudioapi::getSourceEditorContext()
  s <- a$selection[[1]]
  if (s$text=="") {
    rowi <- s$range$start[1]
    rstudioapi::setSelectionRanges(rstudioapi::document_range(c(rowi,0), c(rowi, Inf)))
    a <- rstudioapi::getSourceEditorContext()
    s <- a$selection[[1]]
    rstudioapi::sendToConsole(sprintf("s( %s )", s$text), execute=TRUE, echo=TRUE, focus=FALSE)
  } else {
    rstudioapi::sendToConsole(sprintf("s({\n%s\n})", s$text), execute=TRUE, echo=TRUE, focus=FALSE)
  }
  invisible(TRUE)
}

