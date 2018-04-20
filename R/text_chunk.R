
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

#' Annotation chunk
#'
#' An annotation chunk.
#'
#' @param text text to be displayed.
#'
#' @return
#' @export
#'
#' @examples
#' annotation_chunk("This is a comment on some results.")
annotation_chunk <- function(text) {
  list(
    type = "annotation",
    text = text
  )
}

#' Default function to render an annotation chunk.
#'
#' @param chunk chunk definition.
#' @param sec_path vector of section numbers.
#' @param header whether to treat title as a subsection header (and display it in the table of contents).
#'
#' @return
#' @export
#'
#' @examples
render_annotation_chunk <- function(chunk, sec_path, ...) { # chunk_num, ns, depth=2) {
  annotation_html <- paste(
    '<div class="panel panel-warning">',
    '<div class="panel-heading">Annotation</div>',
    paste0('<div class="panel-body">', chunk$text, '</div>'),
    '</div>',
    sep="\n"
  )
}

