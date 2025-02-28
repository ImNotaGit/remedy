#' Remoter Send
#'
#' send to remote server for remoter package
#'
#' @return invisible(TRUE)
#' @export
remoter_send <- function() {
  a <- rstudioapi::getSourceEditorContext()
  s <- a$selection[[1]]
  if (s$text=="") {
    rowi <- s$range$start[1]
    rstudioapi::setSelectionRanges(rstudioapi::document_range(c(rowi,0), c(rowi, Inf)))
    a <- rstudioapi::getSourceEditorContext()
    s <- a$selection[[1]]
    rstudioapi::modifyRange(location=s$range, text=sprintf("s( %s )", s$text))
    rstudioapi::executeCommand("executeCodeWithoutMovingCursor")
    rstudioapi::setSelectionRanges(rstudioapi::document_range(c(rowi,0), c(rowi, Inf)))
    a <- rstudioapi::getSourceEditorContext()
    s <- a$selection[[1]]
    old.text <- stringr::str_replace(s$text, "^s\\( ", "")
    old.text <- stringr::str_replace(old.text, " \\) *$", "")
    rstudioapi::modifyRange(location=s$range, text=old.text)
    rstudioapi::sendToConsole("", execute=TRUE, echo=FALSE, focus=FALSE)
  } else {
    rstudioapi::modifyRange(location=s$range, sprintf("s({\n%s\n})", s$text))
    rstudioapi::executeCommand("executeCodeWithoutMovingCursor")
    rstudioapi::setSelectionRanges(rstudioapi::document_range(c(s$range$start[1],0), c(s$range$end[1]+2, Inf)))
    a <- rstudioapi::getSourceEditorContext()
    s <- a$selection[[1]]
    old.text <- stringr::str_replace(s$text, "^[ \n]*s\\(\\{\n", "")
    old.text <- stringr::str_replace(old.text, "\n\\}\\)[ \n]*$", "")
    rstudioapi::modifyRange(location=s$range, text=old.text)
    rstudioapi::sendToConsole("", execute=TRUE, echo=FALSE, focus=FALSE)
  }
  invisible(TRUE)
}

