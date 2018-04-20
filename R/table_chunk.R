
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
  depth <- length(sec_path)
  ns <- paste(sec_path, collapse="_")

  #id <- paste0('table', ns)
  num <- gen_id("table")
  id <- paste0('table', num)

  chunk$title <- ifelse(is.null(chunk$title), "", chunk$title)
  chunk$description <- ifelse(is.null(chunk$description), "", chunk$description)
  chunk$scrollX <- ifelse(is.null(chunk$scrollX), FALSE, chunk$scrollX)

  bootstrap_options <- c("responsive", "hover", "condensed")

  # if (chunk$title != "") {
  #   # TODO: make a css style for the chunk titles instead of using hx
  #   # title_html <- h4(paste0("Figure: ", chunk$title))
  #   title_html <- div(class="h4", paste0("Table: ", chunk$title))
  # } else {
  #   title_html <- ""
  # }

  table_html <- kableExtra::kable_styling(
    knitr::kable(chunk$dataframe, format="html"),
    bootstrap_options = bootstrap_options)

  if (chunk$scrollX == TRUE) {
    table_html <- kableExtra::scroll_box(table_html, width="100%")
  }

  # table_html <- paste(table_html, collapse="\n")

  # download
  tabcon <- textConnection("encoded", "w")
  write.table(chunk$dataframe, file = tabcon, sep="\t", row.names=FALSE)
  close(tabcon)
  encoded <- openssl::base64_encode(paste(encoded, collapse="\n"))

  download_html <- a(href=paste0('data:text/csv;base64,', encoded), download=paste0(id, ".csv") , "Download")

  div_class <- if (chunk$collapsed == FALSE) "collapse in" else "collapse"

  if (!is.null(chunk$annotation)) {
    annotation_html <- div(class="panel panel-warning",
                           div(class="panel-heading", "Annotation"),
                           div(class="panel-body", chunk$annotation))

    table_html <- paste(table_html, annotation_html, sep="\n")
  }

  # paste(
  #   title_html,
  #   paste0('<p> ', chunk$description , '</p>'),
  #   '<table style="width:100%"><tr><td style="text-align: left">',
  #   paste0('<a href="#', id, '" data-toggle="collapse">Show/Hide</a>'),
  #   '</td><td style="text-align: right">',
  #   download_html,
  #   '</td></tr></table>',
  #   paste0('<div id="', id, '" class="', div_class,'">'),
  #   table_html,
  #   '</div>',
  #   sep="\n")

  if (chunk$description != "") {
    label_html <- tags$p(tags$b(paste0("Table ", num, ": ", chunk$title, " | ")), HTML(chunk$description))
  } else {
    label_html <- tags$p(tags$b(paste0("Table ", num, ": ", chunk$title)))
  }

  div(class="panel panel-default",
      div(class="panel-body",
          HTML('<table style="width:100%"><tr><td style="text-align: left">'),
          tags$a(href=paste0("#", id), "data-toggle"="collapse", "Show/Hide"),
          HTML('</td><td style="text-align: right">'),
          download_html,
          HTML('</td></tr></table>'),
          div(id=id, class=div_class,
              HTML(table_html)),
          label_html))

  # div(class="panel panel-default",
  #     div(class="panel-body",
  #         tags$p(tags$b(paste0("Table: ", chunk$title, " | ")), HTML(chunk$description)),
  #         HTML('<table style="width:100%"><tr><td style="text-align: left">'),
  #         tags$a(href=paste0("#", id), "data-toggle"="collapse", "Show/Hide"),
  #         HTML('</td><td style="text-align: right">'),
  #         download_html,
  #         HTML('</td></tr></table>'),
  #         div(id=id, class=div_class,
  #             HTML(table_html))))
}

