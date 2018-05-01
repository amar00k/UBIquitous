
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
render_file_chunk <- function(chunk, sec_path, header=TRUE, ...) { #{ chunk_num, ns, depth=2) {
  depth <- length(sec_path)
  ns <- paste(sec_path, collapse="_")

  #id <- paste0('table', ns)
  num <- gen_id("file")
  id <- paste0('file', num)

  chunk$title <- ifelse(is.null(chunk$title), "", chunk$title)
  chunk$description <- ifelse(is.null(chunk$description), "", chunk$description)
  chunk$scrollX <- ifelse(is.null(chunk$scrollX), FALSE, chunk$scrollX)

  # table_html <- kableExtra::kable_styling(
  #   knitr::kable(chunk$dataframe, format="html"),
  #   bootstrap_options = bootstrap_options)
  #
  # if (chunk$scrollX == TRUE) {
  #   table_html <- kableExtra::scroll_box(table_html, width="100%")
  # }

  file_html <- tags$a(href=chunk$uri, chunk$uri)

  # table_html <- paste(table_html, collapse="\n")

  # # download
  # tabcon <- textConnection("encoded", "w")
  # write.table(chunk$dataframe, file = tabcon, sep="\t", row.names=FALSE)
  # close(tabcon)
  # encoded <- openssl::base64_encode(paste(encoded, collapse="\n"))
  #
  # download_html <- a(href=paste0('data:text/csv;base64,', encoded), download=paste0(id, ".csv") , "Download")

  # div_class <- if (chunk$collapsed == FALSE) "collapse in" else "collapse"
  div_class <- "collapse in"

  if (!is.null(chunk$annotation)) {
    annotation_html <- div(class="panel panel-warning",
                           div(class="panel-heading", "Annotation"),
                           div(class="panel-body", chunk$annotation))

    file_html <- paste(file_html, annotation_html, sep="\n")
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
    label_html <- tags$p(tags$b(paste0("File ", num, ": ", chunk$title, " | ")), HTML(chunk$description))
  } else {
    label_html <- tags$p(tags$b(paste0("File ", num, ": ", chunk$title)))
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







#' #' Default function to render a file chunk.
#' #'
#' #' @param chunk chunk definition.
#' #' @param sec_path vector of section numbers.
#' #' @param header whether to treat title as a subsection header (and display it in the table of contents).
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' render_file_chunk <- function(chunk, sec_path, ...) { #{ chunk_num, ns, depth=2) {
#'   depth <- length(sec_path)
#'
#'   chunk$title <- ifelse(is.null(chunk$title), "", chunk$title)
#'   chunk$description <- ifelse(is.null(chunk$description), "", chunk$description)
#'
#'   title_html <- ""
#'   if (chunk$title != "") {
#'     hx <- paste0("h", depth)
#'     title_html <- paste0('<', hx, '> File: ', chunk$title, '</', hx, '>')
#'   }
#'
#'   paste(
#'     title_html,
#'     paste0('<p> ', chunk$description , '</p>'),
#'     paste0('[ Link ](./', chunk$uri, ')'),
#'     sep="\n")
#' }
