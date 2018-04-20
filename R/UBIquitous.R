#' UBIquitous: A Package for Generation of Structured Reports
#'
#' This package allows the user to create HTML reports by specifying a
#' document structure consisting of sections and *chunks*. It includes
#' modules to process NGS datasets.
#'
#' @author Daniel Neves, \email{danielneves2005@gmail.com}
#'
#' @section Chunks:
#' Chunks are displayable elements in a report (e.g. a table, a figure, a paragraph).
#'
#' \code{\link{text_chunk}} specifies a piece of text.
#' \code{\link{table_chunk}} specifies a table.
#' \code{\link{figure_chunk}} specifies a figure.
#'
#' iris_section <- function(num.rows, colors) {
#'   tab1 <- table_chunk(head(iris, num.rows),
#'                       title = "The iris dataset",
#'                       description = paste("This table displays the first", num.rows, "rows of the iris dataset."))
#'
#'   fig1 <- figure_chunk(fun = function() {
#'     pairs(iris, col=colors[ iris$Species ])
#'   }, title = "Pairs plot of iris", description = "")
#'
#'   par = extract_parameters()
#'
#'   section(
#'     title = "Iris",
#'     description = "A section about the iris dataset.",
#'     chunks = list(tab1, fig1),
#'     par = par
#'   )
#' }
#'
#' sec1 <- iris_section(6, colors = c("red", "green", "blue"))
#' document <- list(sections=list(sec1))
#' render_document(document, filename = "Example1.html")
#'
#' @docType package
#' @name UBIquitous-package
NULL

######################
# Internal functions #
######################

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.UBIquitous <- list(
    UBIquitous.chunk_functions = list("table"=render_table_chunk,
                                      "text"=render_text_chunk,
                                      "figure"=render_figure_chunk,
                                      "commands"=render_commands_chunk,
                                      "file"=render_file_chunk,
                                      "annotation"=render_annotation_chunk)

    # UBIquitous.chunks.title_tag
    # devtools.name = "Your name goes here",
    # devtools.desc.author = "First Last <first.last@example.com> [aut, cre]",
    # devtools.desc.license = "What license is it under?",
    # devtools.desc.suggests = NULL,
    # devtools.desc = list()
  )
  toset <- !(names(op.UBIquitous) %in% names(op))
  if(any(toset)) options(op.UBIquitous[toset])

  invisible()
}


.ns.env <- new.env()
assign("ids", list(), envir=.ns.env)

#' Generate a sequencial ID.
#'
#' @return An integer ID number guaranteed to by different from all previously generated IDs.
#' @export
#'
#' @examples
gen_id <- function(type="global") {
  ids <- get("ids", envir=.ns.env)

  if (is.null(ids[[type]]))
    ids[[type]] <- 0

  ids[[type]] <- ids[[type]] + 1
  seqnum <- ids[[type]]

  #seqnum <- get("id", envir=.ns.env) + 1

  assign("ids", ids, envir=.ns.env)

  return(seqnum)
}

####################
# Helper functions #
####################

#' Extract parameters
#'
#' Helper function to make a named list of function parameters and values.
#'
#' @param include.wd Add working directory to parameter list.
#'
#' @return A list of the function's parameters and values.
#' @export
#'
#' @examples
#' my_func <- function(x, a=2, ...) {
#'    extract_parameters()
#' }
#' my_func(10, b=4)
extract_parameters <- function(include.wd = FALSE) {
  caller <- sys.calls()[[sys.nframe()-1]][[1]]

  form <- formals(eval(caller))

  if (!is.null(form)) {
    par <- mget(names(form), sys.frame(sys.nframe()-1))
  } else {
    par <- list()
  }

  if (include.wd == TRUE) {
    par$work.dir <- getwd()
  }

  return(par)
}

##########################
# Chunk render functions #
##########################

html_show_hide <- function(id, contents) {
  # adds a link to show/hide contents
  paste(
    paste0('<a href="#', id, '" data-toggle="collapse">Show/Hide</a>'),
    paste0('<div id="', id, '" class="collapse in">'),
    contents,
    '</div>',
    sep="\n")
}

# render_chunk_params <- function(chunk, chunk_num, ns, depth=2) {
#   id <- paste0(ns, '_params_', chunk_num)
#
#   # make the parameters panel
#   parsdf <- data.frame(Parameter=names(chunk$par), Value=sapply(chunk$par, toString))
#   if (nrow(parsdf) > 0) {
#     pars_html <- paste(
#       '<div class="panel panel-default">',
#       '<div class="panel-heading">',
#       '<h4 class="panel-title">',
#       paste0('<a data-toggle="collapse" href="#', id, '">Show/Hide Parameters</a>'),
#       '</h4>',
#       '</div>',
#       paste0('<div id="', id, '" class="panel-collapse collapse">'),
#       '<div class="panel-body">',
#       as.character(kable_styling(kable(parsdf, row.names = FALSE, format="html"), bootstrap_options = c("condensed"))),
#       '</div></div></div>',
#       sep="\n")
#   } else {
#     pars_html <- ""
#   }
#
#   return(pars_html)
# }

##################
# Render section #
##################


####################
# Module functions #
####################

# # Function to execute a module
# #   - a module returns a list of values
# #   - al values returned are "merged" with those submitted in the parameter (substituting)
# evaluate_module <- function(module) {
#   # msg(txt = paste0("Running: ", module$title))
#
#   # return if module was already evaluated
#   if (!is.null(module$.eval))
#     return(module)
#
#   if (is.null(module$par))
#     module$par <- list()
#
#   # create new env
#   env <- new.env()
#   env$.MAX.THREADS <- 8
#
#   if (!is.null(module$source)) {
#     # source the module in the new env
#     source(module$source, local = env)
#   }
#
#   if (!is.null(module$fun)) {
#     # find the function to call
#     if (class(module$fun) == "character") {
#       fun <- env[[ module$fun ]]
#     } else {
#       fun <- module$fun
#     }
#
#     # call the function
#     module$.eval <- do.call(fun, module$par)
#
#     # run the module function
#     module$env <- env
#     module$chunks <- module$.eval$chunks
#
#     # description is not substituted
#     if (is.null(module$description))
#       module$description <- module$.eval$description
#
#     # parameters are substituted
#     #if (!is.null(module$.eval$par))
#     #  module$par <- module$.eval$par
#     #all_pars <- unique(names(formals(fun)), names(module$par))
#     #module$par <- ifelse(sapply(module$par[ all_pars ], is.null),
#     #                     formals(fun)[ all_pars ],
#     #                     module$par[ all_pars ])
#     #names(module$par) <- all_pars
#   }
#
#   return(module)
# }



#' Render a chunk.
#'
#' TODO: make this output a structure with a print handler
#' right now one has to "cat" the results
#' ideally, in the future, figure_chunk should print itself
#'
#' @param chunk
#'
#' @return
#' @export
#'
#' @examples
render_chunk <- function(chunk) {
  chunk_functions <- getOption("UBIquitous.chunk_functions")

  render_fun <- chunk_functions[[ chunk$type ]]
  chunk_html <- render_fun(chunk, c(0, 1), params=NULL)

  chunk_html <- paste(trimws(strsplit(as.character(chunk_html), split="\n")[[1]]), collapse="\n")

  return(knitr::asis_output(HTML(chunk_html)))
}

#' Title
#'
#' @param chunks
#'
#' @return
#' @export
#'
#' @examples
render_chunks <- function(chunks) {
  chunks_html <- paste(sapply(chunks, render_chunk), collapse="\n")

  return(knitr::asis_output(HTML(chunks_html)))
}


#' Render a single section
#'
#' This function renders a section's HTML.
#'
#' @param section a section (may contain subsections)
#' @param chunk_functions_ex functions used to render chunks (see details).
#' @param section_functions functions used to render sections (a vector, defining the function to use at each section depth).
#'
#' @return
#' @export
#'
#' @examples
render_section <- function(section,
                           title="Untitled",
                           depth=1,
                           render_function = render_section_basic) {
  number_sections=FALSE
  debug=FALSE

  # setup options
  chunk_functions <- getOption("UBIquitous.chunk_functions")

  # # process the chunks
  # process_chunks <- function(chunks, sec_path) {
  #   # set up a namespace for this module
  #   ns <- paste(sec_path, collapse="_") # paste0(parent_ns, "_", sec_num)
  #
  #   # get the *markdown* (not html...) for each chunk
  #   chunks_html <- paste(lapply(seq_along(chunks), function(i) {
  #     x <- chunks[[i]]
  #
  #     paste('<div>',
  #           chunk_functions[[ x$type ]](x, c(sec_path, i), params=NULL), #  i, ns, sec_depth+1),
  #           '</div>',
  #           sep="\n")
  #   }), collapse="\n") # TODO: set chunk separator as parameter
  #
  #   # remove white-space from all lines to prevent markdown generating code blocks
  #   # chunks_html <- paste(trimws(strsplit(as.character(chunks_html), split="\n")[[1]]), collapse="\n")
  #
  #   return(chunks_html)
  # }

  # process a section
  process_section <- function(section, sec_path) {
    # setup namespace for this section
    # sec_num <- rev(sec_path)[1]
    # sec_depth <- length(sec_path)
    # ns <- paste(sec_path, collapse="_") # paste0(parent_ns, "_", sec_num)

    # if (number_sections == TRUE & !is.null(section$title)) {
    #   section$title <- paste(paste(sec_path, collapse="."), section$title) #paste0(gsub('_', '.', substring(ns, 3)), " ", section$title)
    # }

    if (!is.null(section$description)) {
      section_description <- paste0('<p>', section$description, '</p>')
    } else {
      section_description <- ""
    }

    # parameters table
    parsdf <- data.frame(Parameter=names(section$par), Value=sapply(section$par, toString))
    if (nrow(parsdf) > 0) {
      pars_html <- paste(
        as.character(kableExtra::kable_styling(
          knitr::kable(parsdf, row.names = FALSE, format="html"),
          bootstrap_options = c("condensed"))),
        sep="\n")
    } else {
      pars_html <- ""
    }

    # chunks_html <- process_chunks(section$chunks, sec_path) #parent_ns = ns, sec_num = 0, sec_depth = sec_depth)
    chunks_html <- paste(lapply(section$chunks, render_chunk), collapse="\n")

    # process subsections
    # subsections_html <- paste(lapply(seq_along(section$subsections), function(i) {
    #   process_section(section$subsections[[i]], c(sec_path, i))  #parent_ns=ns, sec_num = i, sec_depth = sec_depth+1)
    # }), collapse="\n")

    # must be tested!!
    subsections_html <- paste(lapply(section$subsections, render_section, depth=depth+1), collapse="\n")

    section_body <- paste(chunks_html, subsections_html, sep="\n")

    # this is the html for this section
    if (!is.null(section$render_section))
      render_function <- section$render_section

    out_html <- paste(render_function(ns, depth, section$title, section_description, pars_html, section_body), "\n")
  }

  section_html <- process_section(section, depth)
  out_html <- knitr::asis_output(HTML(section_html))

  return(out_html)
}





#' Render document.
#'
#' This function renders the html document.
#'
#' @param document a section, defining the document structure.
#' @param filename the target filename.
#' @param chunk_functions_ex functions used to render chunks (see details).
#' @param section_functions functions used to render sections (a vector, defining the function to use at each section depth).
#'
#' @return
#' @export
#'
#' @examples
render_document <- function(document, filename,
                            chunk_functions_ex = NULL,
                            section_functions = NULL,
                            title="Untitled",
                            theme="default") {
  require(knitr)
  require(kableExtra)

  number_sections=TRUE
  debug=FALSE

  if (is.null(section_functions)) {
    section_functions <- c(render_section_basic,
                           render_section_panel3,
                           render_section_panel3,
                           render_section_panel3,
                           render_section_panel3)
  }

  # setup options
  chunk_functions = list("table"=render_table_chunk,
                         "text"=render_text_chunk,
                         "figure"=render_figure_chunk,
                         "file"=render_file_chunk,
                         "annotation"=render_annotation_chunk)

  for (n in names(chunk_functions_ex)) {
    chunk_functions[[ n ]] <- chunk_functions_ex[[ n ]]
  }


  params <- list(author="Daniel Neves (dneves@igc.gulbenkian.pt)",
                chunk_functions = chunk_functions,
                section_functions = section_functions)

  # process the chunks
  process_chunks <- function(chunks, sec_path) { #parent_ns, sec_num, sec_depth) {
    # set up a namespace for this module
    ns <- paste(sec_path, collapse="_") # paste0(parent_ns, "_", sec_num)

    # get the *markdown* (not html...) for each chunk
    chunks_html <- paste(lapply(seq_along(chunks), function(i) {
      x <- chunks[[i]]

      paste('<div>',
            chunk_functions[[ x$type ]](x, c(sec_path, i), params=params), #  i, ns, sec_depth+1),
            '</div>',
            sep="\n")
    }), collapse="\n***\n") # TODO: set chunk separator as parameter

    # remove white-space from all lines to prevent markdown generating code blocks
    chunks_html <- paste(trimws(strsplit(as.character(chunks_html), split="\n")[[1]]), collapse="\n")

    return(chunks_html)
  }

  # process a section
  process_section <- function(section, sec_path) {
    # setup namespace for this section
    sec_num <- rev(sec_path)[1]
    sec_depth <- length(sec_path)
    ns <- paste(sec_path, collapse="_") # paste0(parent_ns, "_", sec_num)

    hx <- paste0("h", sec_depth)

    if (number_sections == TRUE & !is.null(section$title)) {
      section$title <- paste(paste(sec_path, collapse="."), section$title) #paste0(gsub('_', '.', substring(ns, 3)), " ", section$title)
    }

    if (!is.null(section$description)) {
      section_description <- paste0('<p>', section$description, '</p><hr/>')
    } else {
      section_description <- ""
    }

    # make the parameters panel
    # if (!is.null(names(section$par)))
    parsdf <- data.frame(Parameter=names(section$par), Value=sapply(section$par, toString))
    if (nrow(parsdf) > 0) {
      pars_html <- paste(
        as.character(kable_styling(kable(parsdf, row.names = FALSE, format="html"), bootstrap_options = c("condensed"))),
        sep="\n")
    } else {
      pars_html <- ""
    }

    chunks_html <- process_chunks(section$chunks, sec_path) #parent_ns = ns, sec_num = 0, sec_depth = sec_depth)

    # process subsections
    subsections_html <- paste(lapply(seq_along(section$subsections), function(i) {
      process_section(section$subsections[[i]], c(sec_path, i))  #parent_ns=ns, sec_num = i, sec_depth = sec_depth+1)
    }), collapse="\n")

    section_body <- paste(chunks_html, subsections_html, sep="\n")

    # this is the html for this section
    if (!is.null(section$render_section))
      render_func <- section$render_section
    else
      render_func <- section_functions[[ sec_depth ]]

    out_html <- paste(render_func(ns, sec_depth, section$title, section_description, pars_html, section_body), "\n")
  }

  # process sections
  # ns <- "0"

  # chunks on the main section
  chunks_html <- process_chunks(document$chunks, 0) # parent_ns = ns, sec_num = 0, sec_depth = 0)

  # process sections
  sections_html <- paste(lapply(seq_along(document$sections), function(i) {
    process_section(document$sections[[i]], i) # ns, i, 1)
  }), collapse="\n")

  out_html <- paste(chunks_html, sections_html, sep="\n")

  rm(params)

  # knit
  rmarkdown::render(system.file("rmd/Default.Rmd", package="UBIquitous"),
                    output_file = basename(filename),
                    output_dir = dirname(filename),
                    knit_root_dir = dirname(filename),
                    quiet = FALSE,
                    output_options = list(
                      theme=theme,
                      toc_float=list(collapsed=TRUE)
                    ),
                    params = list(title=title,
                                  author="Daniel Neves (dneves@igc.gulbenkian.pt)",
                                  out_html=out_html))
}




#' #' Render a single section
#' #'
#' #' This function renders a section's HTML.
#' #'
#' #' @param section a section (may contain subsections)
#' #' @param chunk_functions_ex functions used to render chunks (see details).
#' #' @param section_functions functions used to render sections (a vector, defining the function to use at each section depth).
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' render_section <- function(section,
#'                            chunk_functions_ex = NULL,
#'                            render_function = render_section_basic,
#'                            title="Untitled") {
#'   number_sections=FALSE
#'   debug=FALSE
#'
#'   # if (is.null(section_functions)) {
#'   #   section_functions <- c(render_section_basic,
#'   #                          render_section_panel3,
#'   #                          render_section_panel3,
#'   #                          render_section_panel3,
#'   #                          render_section_panel3)
#'   # }
#'
#'   # setup options
#'   chunk_functions = list("table"=render_table_chunk,
#'                          "text"=render_text_chunk,
#'                          "figure"=render_figure_chunk,
#'                          "file"=render_file_chunk)
#'
#'   for (n in names(chunk_functions_ex)) {
#'     chunk_functions[[ n ]] <- chunk_functions_ex[[ n ]]
#'   }
#'
#'   params <- list(author="Daniel Neves (dneves@igc.gulbenkian.pt)",
#'                  chunk_functions = chunk_functions)
#'
#'   # process the chunks
#'   process_chunks <- function(chunks, sec_path) { #parent_ns, sec_num, sec_depth) {
#'     # set up a namespace for this module
#'     ns <- paste(sec_path, collapse="_") # paste0(parent_ns, "_", sec_num)
#'
#'     # get the *markdown* (not html...) for each chunk
#'     chunks_html <- paste(lapply(seq_along(chunks), function(i) {
#'       x <- chunks[[i]]
#'
#'       paste('<div>',
#'             chunk_functions[[ x$type ]](x, c(sec_path, i), params=params), #  i, ns, sec_depth+1),
#'             '</div>',
#'             sep="\n")
#'     }), collapse="\n") # TODO: set chunk separator as parameter
#'
#'     # remove white-space from all lines to prevent markdown generating code blocks
#'     chunks_html <- paste(trimws(strsplit(as.character(chunks_html), split="\n")[[1]]), collapse="\n")
#'
#'     return(chunks_html)
#'   }
#'
#'   # process a section
#'   process_section <- function(section, sec_path) {
#'     # setup namespace for this section
#'     sec_num <- rev(sec_path)[1]
#'     sec_depth <- length(sec_path)
#'     ns <- paste(sec_path, collapse="_") # paste0(parent_ns, "_", sec_num)
#'
#'     hx <- paste0("h", sec_depth)
#'
#'     if (number_sections == TRUE & !is.null(section$title)) {
#'       section$title <- paste(paste(sec_path, collapse="."), section$title) #paste0(gsub('_', '.', substring(ns, 3)), " ", section$title)
#'     }
#'
#'     if (!is.null(section$description)) {
#'       section_description <- paste0('<p>', section$description, '</p>')
#'     } else {
#'       section_description <- ""
#'     }
#'
#'     # make the parameters panel
#'     # if (!is.null(names(section$par)))
#'     parsdf <- data.frame(Parameter=names(section$par), Value=sapply(section$par, toString))
#'     if (nrow(parsdf) > 0) {
#'       pars_html <- paste(
#'         as.character(kableExtra::kable_styling(
#'           knitr::kable(parsdf, row.names = FALSE, format="html"),
#'           bootstrap_options = c("condensed"))),
#'         sep="\n")
#'     } else {
#'       pars_html <- ""
#'     }
#'
#'     chunks_html <- process_chunks(section$chunks, sec_path) #parent_ns = ns, sec_num = 0, sec_depth = sec_depth)
#'
#'     # process subsections
#'     subsections_html <- paste(lapply(seq_along(section$subsections), function(i) {
#'       process_section(section$subsections[[i]], c(sec_path, i))  #parent_ns=ns, sec_num = i, sec_depth = sec_depth+1)
#'     }), collapse="\n")
#'
#'     section_body <- paste(chunks_html, subsections_html, sep="\n")
#'
#'     # this is the html for this section
#'     if (!is.null(section$render_section))
#'       render_function <- section$render_section
#'
#'     out_html <- paste(render_function(ns, sec_depth, section$title, section_description, pars_html, section_body), "\n")
#'   }
#'
#'   # process sections
#'   # ns <- "0"
#'
#'   # chunks on the main section
#'   # chunks_html <- process_chunks(document$chunks, 0) # parent_ns = ns, sec_num = 0, sec_depth = 0)
#'
#'   # process sections
#'   id <- gen_id("section")
#'   section_html <- process_section(section, id) # ns, i, 1)
#'
#'   # sections_html <- paste(lapply(seq_along(document$sections), function(i) {
#'   #   process_section(document$sections[[i]], i) # ns, i, 1)
#'   # }), collapse="\n")
#'
#'   out_html <- knitr::asis_output(HTML(section_html))
#'
#'   return(out_html)
#' }


#' Render a single section
#'
#' This function renders a section's HTML.
#'
#' @param section a section (may contain subsections)
#' @param chunk_functions_ex functions used to render chunks (see details).
#' @param section_functions functions used to render sections (a vector, defining the function to use at each section depth).
#'
#' @return
#' @export
#'
#' @examples
view_chunks <- function(chunks,
                        chunk_functions_ex = NULL,
                        section_functions = NULL,
                        title="Untitled") {
  require(knitr)
  require(kableExtra)

  number_sections=TRUE
  debug=FALSE

  if (is.null(section_functions)) {
    section_functions <- c(render_section_basic,
                           render_section_panel3,
                           render_section_panel3,
                           render_section_panel3,
                           render_section_panel3)
  }

  # setup options
  chunk_functions = list("table"=render_table_chunk,
                         "text"=render_text_chunk,
                         "figure"=render_figure_chunk,
                         "file"=render_file_chunk)

  for (n in names(chunk_functions_ex)) {
    chunk_functions[[ n ]] <- chunk_functions_ex[[ n ]]
  }

  params <- list(author="Daniel Neves (dneves@igc.gulbenkian.pt)",
                 chunk_functions = chunk_functions,
                 section_functions = section_functions)

  # process the chunks
  process_chunks <- function(chunks, sec_path) { #parent_ns, sec_num, sec_depth) {
    # set up a namespace for this module
    ns <- paste(sec_path, collapse="_") # paste0(parent_ns, "_", sec_num)

    # get the *markdown* (not html...) for each chunk
    chunks_html <- paste(lapply(seq_along(chunks), function(i) {
      x <- chunks[[i]]

      paste('<div>',
            chunk_functions[[ x$type ]](x, c(sec_path, i), params=params), #  i, ns, sec_depth+1),
            '</div>',
            sep="\n")
    }), collapse="\n***\n") # TODO: set chunk separator as parameter

    return(chunks_html)
  }

  # process a section
  process_section <- function(section, sec_path) {
    # setup namespace for this section
    sec_num <- rev(sec_path)[1]
    sec_depth <- length(sec_path)
    ns <- paste(sec_path, collapse="_") # paste0(parent_ns, "_", sec_num)

    hx <- paste0("h", sec_depth)

    if (number_sections == TRUE & !is.null(section$title)) {
      section$title <- paste(paste(sec_path, collapse="."), section$title) #paste0(gsub('_', '.', substring(ns, 3)), " ", section$title)
    }

    if (!is.null(section$description)) {
      section_description <- paste0('<p>', section$description, '</p><hr/>')
    } else {
      section_description <- ""
    }

    # make the parameters panel
    # if (!is.null(names(section$par)))
    parsdf <- data.frame(Parameter=names(section$par), Value=sapply(section$par, toString))
    if (nrow(parsdf) > 0) {
      pars_html <- paste(
        as.character(kable_styling(kable(parsdf, row.names = FALSE, format="html"), bootstrap_options = c("condensed"))),
        sep="\n")
    } else {
      pars_html <- ""
    }

    chunks_html <- process_chunks(section$chunks, sec_path) #parent_ns = ns, sec_num = 0, sec_depth = sec_depth)

    # process subsections
    subsections_html <- paste(lapply(seq_along(section$subsections), function(i) {
      process_section(section$subsections[[i]], c(sec_path, i))  #parent_ns=ns, sec_num = i, sec_depth = sec_depth+1)
    }), collapse="\n")

    section_body <- paste(chunks_html, subsections_html, sep="\n")

    # this is the html for this section
    if (!is.null(section$render_section))
      render_func <- section$render_section
    else
      render_func <- section_functions[[ sec_depth ]]

    out_html <- paste(render_func(ns, sec_depth, section$title, section_description, pars_html, section_body), "\n")
  }

  # process sections
  # ns <- "0"

  # chunks on the main section
  chunks_html <- process_chunks(chunks, 0) # parent_ns = ns, sec_num = 0, sec_depth = 0)

  # process sections
  # section_html <- process_section(section, 0) # ns, i, 1)

  # sections_html <- paste(lapply(seq_along(document$sections), function(i) {
  #   process_section(document$sections[[i]], i) # ns, i, 1)
  # }), collapse="\n")

  out_html <- chunks_html

  #return(out_html)
  rm(params)

  # knit
  filename <- tempfile(fileext = ".html")
  rmarkdown::render(system.file("rmd/Default.Rmd", package="UBIquitous"),
                    output_file = basename(filename),
                    output_dir = dirname(filename),
                    knit_root_dir = dirname(filename),
                    quiet = FALSE,
                    output_options = list(
                      theme="default",
                      toc_float=list(collapsed=TRUE)
                    ),
                    params = list(title="Chunks Preview",
                                  out_html=out_html))
  viewer <- getOption("viewer")
  viewer(filename)
}
