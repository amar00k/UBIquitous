
#' Text chunk
#'
#' A text chunk displays some text.
#'
#' @param text text to be displayed.
#'
#' @return
#' @export
#'
#' @examples
#' text_chunk("Hello there.")
#' text_chunk("A bit of *text* with some **markdown**")
text_chunk <- function(text) {
  list(
    type = "text",
    text = text
  )
}

#' Default function to render a text chunk.
#'
#' @param chunk chunk definition.
#' @param sec_path vector of section numbers.
#' @param header whether to treat title as a subsection header (and display it in the table of contents).
#'
#' @return
#' @export
#'
#' @examples
render_text_chunk <- function(chunk, sec_path, ...) { # chunk_num, ns, depth=2) {
  paste('<p>', chunk$text, '</p>')
}

