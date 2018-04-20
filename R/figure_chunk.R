




#' Figure chunk
#'
#' A figure chunk displays a figure.
#'
#' @param fun function to generate the figure.
#' @param title the title of the figure.
#' @param description description (or caption) of the figure to be displayed right below the title.
#' @param width width of the figure in inches
#' @param height height of the figure in inches
#' @param pdf.filename If specified, a PDF will be automatically generated and linked from the main document.
#'
#' @return
#' @export
#'
#' @examples
figure_chunk <- function(fun,
                         title = "",
                         description = "",
                         width=6, height=5,
                         pdf.filename=NULL,
                         collapsed = FALSE,
                         ...) {
  list(
    type = "figure",
    fun = fun,
    title = title,
    width = width,
    height = height,
    description = description,
    pdf.filename = pdf.filename,
    collapsed = collapsed
  )
}

#' Default function to render a figure chunk.
#'
#' @param chunk chunk definition.
#' @param sec_path vector of section numbers.
#' @param header whether to treat title as a subsection header (and display it in the table of contents).
#'
#' @return
#' @export
#'
#' @import htmltools
#'
#' @examples
render_figure_chunk <- function(chunk, sec_path, header=TRUE, ...) {
  num <- gen_id("figure")
  id <- paste0('figure', num)

  chunk$title <- ifelse(is.null(chunk$title), "", chunk$title)
  chunk$description <- ifelse(is.null(chunk$description), "", chunk$description)
  chunk$width <- ifelse(is.null(chunk$width), 6, chunk$width)
  chunk$height <- ifelse(is.null(chunk$height), 6, chunk$height)

  # if (chunk$title != "") {
  #   # TODO: make a css style for the chunk titles instead of using hx
  #   # title_html <- h4(paste0("Figure: ", chunk$title))
  #   title_html <- div(class="h4", paste0("Figure: ", chunk$title))
  # } else {
  #   title_html <- ""
  # }

  # generate the figure png
  tmp <- tempfile(fileext = ".png")

  png(tmp, width=chunk$width, height=chunk$height, units = "in", res=96)
  chunk$fun()
  dev.off()

  # if no file was produced, try "plotting" the result of the plooting function (to handle ggplot and others)
  if (!file.exists(tmp)) {
    png(tmp, width=chunk$width, height=chunk$height, units = "in", res=96)
    plot(chunk$fun())
    dev.off()
  }

  # if still no file was produced, show an error
  if (!file.exists(tmp)) {
    figure_html <- div(class="alert alert-danger", "Error: could not make figure.")
  } else {
    # encode into base64
    encoded <- openssl::base64_encode(readBin(tmp, "raw", file.info(tmp)[1, "size"]))

    figure_html <- img(src=paste0('data:image/png;base64,', encoded))
  }

  # generate the figure pdf
  if (!is.null(chunk$pdf.filename)) {
    pdf(file = chunk$pdf.filename, width=chunk$width, height=chunk$height)
    chunk$fun()
    dev.off()

    download_html <- a(href=chunk$pdf.filename, "Download PDF")
  } else {
    download_html <- ""
  }

  div_class <- if (chunk$collapsed == FALSE) "collapse in" else "collapse"

  if (!is.null(chunk$annotation)) {
    annotation_html <- div(class="panel panel-warning",
                           div(class="panel-heading", "Annotation"),
                           div(class="panel-body", chunk$annotation))

    figure_html <- div(class="container-fluid",
                       div(class="row", style="display: flex; align-items: center",
                           div(class="col-xs-8", figure_html),
                           div(class="col-xs-4", annotation_html)))
  }

  if (chunk$description != "") {
    label_html <- tags$p(tags$b(paste0("Figure ", num, ": ", chunk$title, " | ")), HTML(chunk$description))
  } else {
    label_html <- tags$p(tags$b(paste0("Figure ", num, ": ", chunk$title)))
  }

  # div(
  #   title_html,
  #   p(chunk$description),
  #   HTML('<table style="width:100%"><tr><td style="text-align: left">'),
  #   a(href=paste0("#", id), "data-toggle"="collapse", "Show/Hide"),
  #   HTML('</td><td style="text-align: right">'),
  #   download_html,
  #   HTML('</td></tr></table>'),
  #   div(id=id, class=div_class, figure_html))

  html_panel(
    tagList(
      HTML('<table style="width:100%"><tr><td style="text-align: left">'),
      tags$a(href=paste0("#", id), "data-toggle"="collapse", "Show/Hide"),
      HTML('</td><td style="text-align: right">'),
      download_html,
      HTML('</td></tr></table>'),
      div(id=id, class=div_class,
          figure_html),
      label_html))

  # div(    HTML('<table style="width:100%"><tr><td style="text-align: left">'),
  #         tags$a(href=paste0("#", id), "data-toggle"="collapse", "Show/Hide"),
  #         HTML('</td><td style="text-align: right">'),
  #         download_html,
  #         HTML('</td></tr></table>'),
  #         div(id=id, class=div_class, figure_html),
  #         tags$p(tags$b(paste0("Figure: ", chunk$title, " | ")), HTML(chunk$description)))
}






#'
#' #' Figure chunk
#' #'
#' #' A figure chunk displays a figure.
#' #'
#' #' @param fun function to generate the figure.
#' #' @param title the title of the figure.
#' #' @param description description (or caption) of the figure to be displayed right below the title.
#' #' @param width width of the figure in inches
#' #' @param height height of the figure in inches
#' #' @param pdf.filename If specified, a PDF will be automatically generated and linked from the main document.
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' figure_chunk <- function(fun,
#'                          title = "",
#'                          description = "",
#'                          width=6, height=5,
#'                          pdf.filename=NULL,
#'                          collapsed = FALSE) {
#'   list(
#'     type = "figure",
#'     fun = fun,
#'     title = title,
#'     width = width,
#'     height = height,
#'     description = description,
#'     pdf.filename = pdf.filename,
#'     collapsed = collapsed
#'   )
#' }
#'
#' #' Default function to render a figure chunk.
#' #'
#' #' @param chunk chunk definition.
#' #' @param sec_path vector of section numbers.
#' #' @param header whether to treat title as a subsection header (and display it in the table of contents).
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' render_figure_chunk <- function(chunk, sec_path, header=TRUE, ...) { #chunk_num, ns, depth=2) {
#'   depth <- length(sec_path)
#'   ns <- paste(sec_path, collapse="_")
#'   id <- paste0('figure', ns)
#'
#'   chunk$title <- ifelse(is.null(chunk$title), "", chunk$title)
#'   chunk$description <- ifelse(is.null(chunk$description), "", chunk$description)
#'   chunk$width <- ifelse(is.null(chunk$width), 6, chunk$width)
#'   chunk$height <- ifelse(is.null(chunk$height), 6, chunk$height)
#'
#'   title_html <- ""
#'   if (chunk$title != "") {
#'     hx <- paste0("h", depth)
#'
#'     if (header == TRUE) {
#'       title_html <- paste0('<', hx, '> Figure: ', chunk$title, '</', hx, '>')
#'     } else {
#'       title_html <- paste0('<div class="', hx, '"> Figure: ', chunk$title, '</div>')
#'     }
#'   }
#'
#'   # generate the figure png
#'   tmp <- tempfile(fileext = ".png")
#'
#'   png(tmp, width=chunk$width, height=chunk$height, units = "in", res=96)
#'   chunk$fun()
#'   dev.off()
#'
#'   # if no file was produced, try "plotting" the result of the plooting function (to handle ggplot and others)
#'   if (!file.exists(tmp)) {
#'     png(tmp, width=chunk$width, height=chunk$height, units = "in", res=96)
#'     plot(chunk$fun())
#'     dev.off()
#'   }
#'
#'   # if still no file was produced, show an error
#'   if (!file.exists(tmp)) {
#'     figure_html <- '<div class="alert alert-danger"> Error making figure. </div>'
#'   } else {
#'     # encode into base64
#'     encoded <- openssl::base64_encode(readBin(tmp, "raw", file.info(tmp)[1, "size"]))
#'
#'     figure_html <- paste0('![](', paste0('data:image/png;base64,', encoded), ')\n', sep="")
#'   }
#'
#'   # generate the figure pdf
#'   if (!is.null(chunk$pdf.filename)) {
#'     pdf(file = chunk$pdf.filename, width=chunk$width, height=chunk$height)
#'     chunk$fun()
#'     dev.off()
#'
#'     download_html <- paste0('<a href="', chunk$pdf.filename, '">Download PDF</a>')
#'   } else {
#'     download_html <- ""
#'   }
#'
#'   div_class <- if (chunk$collapsed == FALSE) "collapse in" else "collapse"
#'
#'   side.by.side <- function(content1, content2) {
#'     paste(
#'       '<div class="container-fluid">',
#'       '<div class="row" style="display: flex; align-items: center">',
#'       '<div class="col-xs-8">',
#'       content1,
#'       '</div>',
#'       '<div class="col-xs-4">',
#'       content2,
#'       '</div>',
#'       '</div>',
#'       '</div>',
#'       sep="\n"
#'     )
#'   }
#'
#'   if (!is.null(chunk$annotation)) {
#'     annotation_html <- paste(
#'       '<div class="panel panel-warning">',
#'       '<div class="panel-heading">Annotation</div>',
#'       paste0('<div class="panel-body">', chunk$annotation, '</div>'),
#'       '</div>',
#'       sep="\n"
#'     )
#'
#'     figure_html <- side.by.side(figure_html, annotation_html)
#'   }
#'
#'   paste(
#'     title_html,
#'     paste0('<p> ', chunk$description , '</p>'),
#'     '<table style="width:100%"><tr><td style="text-align: left">',
#'     paste0('<a href="#', id, '" data-toggle="collapse">Show/Hide</a>'),
#'     '</td><td style="text-align: right">',
#'     download_html,
#'     '</td></tr></table>',
#'     paste0('<div id="', id, '" class="', div_class,'">'),
#'     figure_html,
#'     '</div>',
#'     sep="\n")
#' }


# render_figure_chunk_2 <- function(chunk, sec_path, header=TRUE, ...) { #chunk_num, ns, depth=2) {
#   depth <- length(sec_path)
#   ns <- paste(sec_path, collapse="_")
#   id <- paste0('figure', ns)
#
#   chunk$title <- ifelse(is.null(chunk$title), "", chunk$title)
#   chunk$description <- ifelse(is.null(chunk$description), "", chunk$description)
#   chunk$width <- ifelse(is.null(chunk$width), 6, chunk$width)
#   chunk$height <- ifelse(is.null(chunk$height), 6, chunk$height)
#
#   title_html <- ""
#   if (chunk$title != "") {
#     hx <- paste0("h", depth)
#
#     if (header == TRUE) {
#       title_html <- paste0('<', hx, '> Figure: ', chunk$title, '</', hx, '>')
#     } else {
#       title_html <- paste0('<div class="', hx, '"> Figure: ', chunk$title, '</div>')
#     }
#   }
#
#   # generate the figure png
#   tmp <- tempfile(fileext = ".png")
#
#   png(tmp, width=chunk$width, height=chunk$height, units = "in", res=96)
#   chunk$fun()
#   dev.off()
#
#   if (!file.exists(tmp)) {
#     figure_html <- '<div class="alert alert-danger"> Error making figure. </div>'
#   } else {
#     # encode into base64
#     encoded <- openssl::base64_encode(readBin(tmp, "raw", file.info(tmp)[1, "size"]))
#
#     figure_html <- paste0('![](', paste0('data:image/png;base64,', encoded), ')\n', sep="")
#   }
#
#   # generate the figure pdf
#   if (!is.null(chunk$pdf.filename)) {
#     pdf(file = chunk$pdf.filename, width=chunk$width, height=chunk$height)
#     chunk$fun()
#     dev.off()
#
#     download_html <- paste0('<a href="', chunk$pdf.filename, '">Download PDF</a>')
#   } else {
#     download_html <- ""
#   }
#
#   div_class <- if (chunk$collapsed == FALSE) "collapse in" else "collapse"
#
#   paste(
#     title_html,
#     paste0('<p> ', chunk$description , '</p>'),
#     '<table style="width:100%"><tr><td style="text-align: left">',
#     paste0('<a href="#', id, '" data-toggle="collapse">Show/Hide</a>'),
#     '</td><td style="text-align: right">',
#     download_html,
#     '</td></tr></table>',
#     paste0('<div id="', id, '" class="', div_class,'">'),
#     figure_html,
#     '</div>',
#     sep="\n")
# }
