#' Toggle Send
#'
#' add or remove s() for remoter package
#'
#' @return modified text selection
#' @export
toggle_s <- function() {
  a <- rstudioapi::getSourceEditorContext()
  s <- a$selection[[1]]
  if (s$text=="") {
    rowi <- s$range$start[1]
    rstudioapi::setSelectionRanges(rstudioapi::document_range(c(rowi,0), c(rowi, Inf)))
    a <- rstudioapi::getSourceEditorContext()
    s <- a$selection[[1]]
    if (grepl("^s\\( ", s$text) && grepl(" \\) *$", s$text)) {
      new.text <- stringr::str_replace(s$text, "^s\\( ", "")
      new.text <- stringr::str_replace(new.text, " \\) *$", "")
      rstudioapi::modifyRange(location=s$range, text=new.text)
    } else {
      rstudioapi::modifyRange(location=s$range, text=sprintf("s( %s )", s$text))
    }
  } else {
    if (grepl("^[ \n]*s\\(\\{\n", s$text) && grepl("\n\\}\\)[ \n]*$", s$text)) {
      new.text <- stringr::str_replace(s$text, "^[ \n]*s\\(\\{\n", "")
      new.text <- stringr::str_replace(new.text, "\n\\}\\)[ \n]*$", "")
      rstudioapi::modifyRange(location=s$range, text=new.text)
    } else {
      rstudioapi::modifyRange(location=s$range, text=sprintf("s({\n%s\n})\n", s$text))
    }
  }
}

