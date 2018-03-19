
#' File chunk
#'
#' @param uri
#' @param title
#' @param description
#'
#' @return
#' @export
#'
#' @examples
file_chunk <- function(uri, title = "", description = "") {
  list(
    type = "file",
    uri = uri,
    title = title,
    description = description
  )
}


#' Default function to render a file chunk.
#'
#' @param chunk chunk definition.
#' @param sec_path vector of section numbers.
#' @param header whether to treat title as a subsection header (and display it in the table of contents).
#'
#' @return
#' @export
#'
#' @examples
render_file_chunk <- function(chunk, sec_path, ...) { #{ chunk_num, ns, depth=2) {
  depth <- length(sec_path)

  chunk$title <- ifelse(is.null(chunk$title), "", chunk$title)
  chunk$description <- ifelse(is.null(chunk$description), "", chunk$description)

  title_html <- ""
  if (chunk$title != "") {
    hx <- paste0("h", depth)
    title_html <- paste0('<', hx, '> File: ', chunk$title, '</', hx, '>')
  }

  paste(
    title_html,
    paste0('<p> ', chunk$description , '</p>'),
    paste0('[ Link ](./', chunk$uri, ')'),
    sep="\n")
}
