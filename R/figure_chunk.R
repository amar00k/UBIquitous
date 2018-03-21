
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
figure_chunk <- function(fun, title = "", description = "", width=6, height=5, pdf.filename=NULL, collapsed = FALSE) {
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
#' @examples
render_figure_chunk <- function(chunk, sec_path, header=TRUE, ...) { #chunk_num, ns, depth=2) {
  depth <- length(sec_path)
  ns <- paste(sec_path, collapse="_")
  id <- paste0('figure', ns)

  chunk$title <- ifelse(is.null(chunk$title), "", chunk$title)
  chunk$description <- ifelse(is.null(chunk$description), "", chunk$description)
  chunk$width <- ifelse(is.null(chunk$width), 6, chunk$width)
  chunk$height <- ifelse(is.null(chunk$height), 6, chunk$height)

  title_html <- ""
  if (chunk$title != "") {
    hx <- paste0("h", depth)

    if (header == TRUE) {
      title_html <- paste0('<', hx, '> Figure: ', chunk$title, '</', hx, '>')
    } else {
      title_html <- paste0('<div class="', hx, '"> Figure: ', chunk$title, '</div>')
    }
  }

  # generate the figure png
  tmp <- tempfile(fileext = ".png")

  png(tmp, width=chunk$width, height=chunk$height, units = "in", res=96)
  chunk$fun()
  dev.off()

  if (!file.exists(tmp)) {
    chunk_rmd <- '<div class="alert alert-danger"> Error making figure. </div>'
  } else {
    # encode into base64
    encoded <- openssl::base64_encode(readBin(tmp, "raw", file.info(tmp)[1, "size"]))

    chunk_rmd <- paste0('![](', paste0('data:image/png;base64,', encoded), ')\n', sep="")
  }

  # generate the figure pdf
  if (!is.null(chunk$pdf.filename)) {
    pdf(file = chunk$pdf.filename, width=chunk$width, height=chunk$height)
    chunk$fun()
    dev.off()

    download_html <- paste0('<a href="', chunk$pdf.filename, '">Download PDF</a>')
  } else {
    download_html <- ""
  }

  paste(
    title_html,
    paste0('<p> ', chunk$description , '</p>'),
    '<table style="width:100%"><tr><td style="text-align: left">',
    paste0('<a href="#', id, '" data-toggle="collapse">Show/Hide</a>'),
    '</td><td style="text-align: right">',
    download_html,
    '</td></tr></table>',
    paste0('<div id="', id, '" class="collapse in">'),
    chunk_rmd,
    '</div>',
    sep="\n")

}


