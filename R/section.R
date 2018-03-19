

#' Define a section.
#'
#' A section has a title, chunks to display content, and may contain subsections.
#'
#' @param title the title of the section.
#' @param chunks list of chunks to display.
#' @param subsections list of subsections.
#' @param par optional list of parameters to display in header.
#' @param description description of the section, to display below the title.
#'
#' @return
#' @export
#'
#' @examples
section <- function(title, chunks=NULL, subsections=NULL, par=NULL, description="") {
  list(
    title = title,
    description = description,
    chunks = chunks,
    subsections = subsections,
    par = par)
}

#' Source a section from a file.
#'
#' Convenience function to create a section by sourcing an script.
#'
#' @param title the title of the section.
#' @param sourcename the file containing the section function.
#' @param funname name of the function to call.
#' @param params parameters to pass to the function
#' @param description description of the section (optional). If not NULL will replace the description provided by the function.
#'
#' @return
#' @export
#'
#' @examples
source_section <- function(title, sourcename, funname, params=list(), description=NULL) {
  # source the module in the new env
  env <- new.env()
  source(sourcename, local = env)

  # find the function to call
  fun <- env[[ funname ]]

  # call the function
  section <- do.call(fun, params)

  # run the module function
  section$env <- env

  section$title <- title

  # description is not substituted
  if (!is.null(description))
    section$description <- description

  return(section)
}


#' A basic section style.
#'
#' This is an internal function not meant to be called by the user.
#'
#' @param ns
#' @param depth
#' @param title
#' @param description
#' @param parameters
#' @param body
#'
#' @return
#' @export
#'
#' @examples
render_section_basic <- function(ns, depth, title, description, parameters, body) {
  hx <- paste0("h", depth)
  id <- paste0(ns, "_section")

  section_header <- paste0('<', hx, '>', title, '</', hx, '>')

  if (parameters != "") {
    pars_html <- paste(
      paste0('<a data-toggle="collapse" href="#', id, '">Show/Hide Parameters</a>'),
      paste0('<div id="', id, '" class="panel-collapse collapse">'),
      '<div class="panel-body">',
      parameters,
      '</div></div>',
      sep="\n")
  } else {
    pars_html <- ""
  }

  panel_string <- paste(
    '<div>',
    section_header,
    pars_html,
    paste0('<p>', description, '</p>'),
    body,
    '</div>',
    sep="\n")
}

#' A nice panel style for sections.
#'
#' @param ns
#' @param depth
#' @param title
#' @param description
#' @param parameters
#' @param body
#'
#' @return
#' @export
#'
#' @examples
render_section_panel3 <- function(ns, depth, title, description, parameters, body) {
  hx <- paste0("h", depth)

  id <- paste0(ns, "_section")

  if (parameters != "") {
    pars_html <- paste(
      '<div class="panel panel-default">',
      '<div class="panel-heading">',
      '<h4 class="panel-title">',
      paste0('<a data-toggle="collapse" href="#', id, '">Show/Hide Parameters</a>'),
      '</h4>',
      '</div>',
      paste0('<div id="', id, '" class="panel-collapse collapse">'),
      '<div class="panel-body">',
      parameters,
      '</div></div></div>',
      sep="\n")
  } else {
    pars_html <- ""
  }

  if (!is.null(title)) {
    panel_string <- paste(
      '<div class="panel panel-primary">',
      paste0('<', hx, ' class="panel-heading panel-title"><b>', title, '</b></', hx, '>'),
      '<div class="panel-body">',
      pars_html,
      paste0('<p>', description, '</p>'),
      body,
      '</div></div>',
      sep="\n")
  } else {
    panel_string <- body
  }
}

render_section_panel <- function(ns, depth, title, description, parameters, body) {
  hx <- paste0("h", depth)
  section_header <- paste0('<', hx, '>', title, '</', hx, '>')

  panel_string <- paste(
    section_header,
    '<div class="panel panel-default"><div class="panel-body">',
    paste0('<p>', description, '</p>'),
    parameters,
    body,
    '</div></div>',
    sep="\n")
}

render_section_panel2 <- function(ns, depth, title, description, parameters, body) {
  hx <- paste0("h", depth)
  section_header <- paste0('<', hx, '>', title, '</', hx, '>')

  panel_string <- paste(
    '<div class="panel panel-default"><div class="panel-body">',
    section_header,
    paste0('<p>', description, '</p>'),
    parameters,
    body,
    '</div></div>',
    sep="\n")
}



render_section_collapse_panel <- function(ns, depth, title, description, parameters, body) {
  hx <- paste0("h", depth)

  panel_string <- paste(
    paste0('<div class="panel panel-primary" id="sec_', ns, '">'),
    paste0('<', hx, ' class="panel-heading panel-title">'),
    paste0('<a data-toggle="collapse" href="#', ns, '_main"><b>', title, '</b></a>'),
    paste0('</', hx, '>'),
    paste0('<div id="', ns, '_main" class="panel-collapse collapse in">'),
    '<div class="panel-body">',
    paste0('<p>', description, '</p>'),
    parameters,
    body,
    '</div></div></div>',
    sep="\n")
}


