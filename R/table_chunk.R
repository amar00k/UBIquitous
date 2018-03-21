
#' Table chunk
#'
#' A table chunk displays a table. By default, the table will be generated using \code{\link{kable}} and \code{\link{kable_styling}}.
#'
#' @param dataframe the table to display.
#' @param title the title of the table.
#' @param description description (or caption) of the table to be displayed right below the title.
#' @param scrollX whether add a horizontal scrollbar to the table
#'
#' @return
#' @export
#'
#' @examples
table_chunk <- function(dataframe, title = "", description = "", scrollX = FALSE, collapsed = FALSE) {
  list(
    type = "table",
    dataframe = dataframe,
    title = title,
    description = description,
    scrollX = scrollX,
    collapsed = collapsed
  )
}

#' Default function to render a table chunk.
#'
#' @param chunk chunk definition.
#' @param sec_path vector of section numbers.
#' @param header whether to treat title as a subsection header (and display it in the table of contents).
#'
#' @return
#' @export
#'
#' @examples
render_table_chunk <- function(chunk, sec_path, header=TRUE, ...) { #{ chunk_num, ns, depth=2) {
  require(knitr)
  require(kableExtra)

  depth <- length(sec_path)
  ns <- paste(sec_path, collapse="_")
  id <- paste0('table', ns)

  chunk$title <- ifelse(is.null(chunk$title), "", chunk$title)
  chunk$description <- ifelse(is.null(chunk$description), "", chunk$description)
  chunk$scrollX <- ifelse(is.null(chunk$scrollX), FALSE, chunk$scrollX)

  #bootstrap_options_str <- 'c("striped", "hover", "condensed", "responsive")'
  bootstrap_options <- c("hover", "condensed", "responsive")

  title_html <- ""
  if (chunk$title != "") {
    hx <- paste0("h", depth)

    if (header == TRUE) {
      title_html <- paste0('<', hx, '> Table: ', chunk$title, '</', hx, '>')
    } else {
      title_html <- paste0('<div class="', hx, '"> Table: ', chunk$title, '</div>')
    }
  }

  table_html <- kableExtra::kable_styling(
    knitr::kable(chunk$dataframe, format="html"),
    bootstrap_options = bootstrap_options)

  if (chunk$scrollX == TRUE) {
    table_html <- kableExtra::scroll_box(table_html, width="100%")
  }

  table_html <- paste(table_html, collapse="\n")

  # download
  tabcon <- textConnection("encoded", "w")
  write.table(chunk$dataframe, file = tabcon, sep="\t", row.names=FALSE)
  close(tabcon)
  encoded <- openssl::base64_encode(paste(encoded, collapse="\n"))

  download_html <- paste0('<a href="', paste0('data:text/csv;base64,', encoded), '">Download</a>')

  div_class <- if (chunk$collapsed == FALSE) "collapse in" else "collapse"

  paste(
    title_html,
    paste0('<p> ', chunk$description , '</p>'),
    '<table style="width:100%"><tr><td style="text-align: left">',
    paste0('<a href="#', id, '" data-toggle="collapse">Show/Hide</a>'),
    '</td><td style="text-align: right">',
    download_html,
    '</td></tr></table>',
    paste0('<div id="', id, '" class="', div_class,'">'),
    table_html,
    '</div>',
    sep="\n")
}

