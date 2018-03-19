

#' Tabs chunk.
#'
#' The tabs chunk displays other chunks inside tabs.
#'
#' @param tabchunks
#' @param tabnames
#' @param title
#' @param description
#'
#' @return
#' @export
#'
#' @examples
tabs_chunk <- function(tabchunks, tabnames, title = "", description = "") {
  list(
    type = "tabs",
    tabchunks = tabchunks,
    tabnames = tabnames,
    title = title,
    description = description
  )
}


#' Default function to render a tabs chunk.
#'
#' @param chunk chunk definition.
#' @param sec_path vector of section numbers.
#' @param header whether to treat title as a subsection header (and display it in the table of contents).
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
render_tabs_chunk <- function(chunk, sec_path, params, ...) {
  depth <- length(sec_path)
  ns <- paste(sec_path, collapse="_")
  id <- paste0('table', ns)

  ntabs <- length(chunk$tabchunks)

  # header
  title_html <- ""
  if (chunk$title != "") {
    hx <- paste0("h", depth)
    title_html <- paste0('<', hx, '>', chunk$title, '</', hx, '>')
  }

  # tab buttons
  li <- paste0('<li', c(' class="active"', rep("", ntabs-1)), '>',
               '<a data-toggle="tab" href="#menu', 1:ntabs ,'">Menu ', 1:ntabs,'</a></li>',
               collapse="\n")
  li <- paste('<ul class="nav nav-tabs">', li, '</ul>', sep="\n")

  # tab content
  tabhtml <- lapply(1:ntabs, function(i) {
    x <- chunk$tabchunks[[i]]

    params$chunk_functions[[ x$type ]](x, c(sec_path, i*100), params=params, header=FALSE) # we must be careful with the sec_path here...
  })

  content <- paste0('<div id="menu', 1:ntabs,'" class="tab-pane fade ', c('in active', rep("", ntabs-1)), '">',
                    tabhtml,
                    '</div>',
                    collapse="\n")

  content <- paste('<div class="tab-content">', content, '</div>', sep="\n")

  paste(title_html, li, content, sep="\n")

}

