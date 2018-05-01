

# chunk_base <- function()
encode_dataframe <- function(dataframe) {
  tabcon <- textConnection("encoded", "w")
  write.table(dataframe, file = tabcon, sep="\t", row.names=FALSE)
  close(tabcon)
  openssl::base64_encode(paste(encoded, collapse="\n"))
}

#' Title
#'
#' @param title
#' @param dataframe
#'
#' @return
#' @export
#'
#' @examples
ext_table <-function(title, dataframe, description) {
  list(
    type="ext_table",
    dataframe=dataframe,
    title=title,
    description=description
  )
}

render_ext_table <- function(id, dataframe, title, description) {
  encoded <- encode_dataframe(dataframe)

  as.character(tags$li(tags$a(href=paste0('data:text/csv;base64,', encoded), download=paste0(id, ".csv") , title),
          " : ", description))
}

#' Make an HTML link to a table.
#'
#' @param x a dataframe or a character string indicating a file name.
#' @param filename
#' @param text
#'
#' @return
#' @export
#'
#' @examples
#' # embeds iris encoded in base64 and makes a link
#' table_link(iris, text="Iris")
#'
#' # saves iris to a file (as tab-separated values) and makes a link
#' table_link(iris, text="Iris", filename="html_files/iris.tsv")
#'
#' # includes a link to my_table.tsv
#' table_link("my_table.tsv", text="My table")
table_link <- function(x, text="Table", filename=NULL) {

}


#' Run shell commands with logging
#'
#' @param commands
#' @param log.basename
#' @param append
#'
#' @return
#' @export
#'
#' @examples
run_system <- function(commands, log.basename, append=TRUE) {
  # make sure commands are single lines
  commands <- unlist(strsplit(commands, split="\n"))

  # abort if commands is empty
  if (length(commands) == 0)
    return()

  # make sure the target directory for the logs exists, if not create it
  if (!dir.exists(dirname(log.basename))) {
    dir.create(dirname(log.basename), recursive = TRUE)
  }

  commands.filename <- paste0(log.basename, ".commands.sh")
  stdout.filename <- paste0(log.basename, ".stdout.txt")
  stderr.filename <- paste0(log.basename, ".stderr.txt")

  timestamp <- paste("#", date())
  write(timestamp, file = commands.filename, append=append)
  write(commands, file = commands.filename, append=append)

  sapply(commands, function(x) {
    # write the commands to stdout and stderr files prefixed with +
    timestamp <- paste("+", date())

    write(paste(timestamp, ":", x), stdout.filename, append=TRUE)
    write(paste(timestamp, ":", x), stderr.filename, append=TRUE)

    # Run each command in the form:
    # $ (command) 1>> stdout 2>> stderr
    system(paste0("(", x, ")", " 1>> ", stdout.filename, " 2>> ", stderr.filename))
  })
}

#' Run shell commands with logging
#'
#' @param commands
#' @param log.basename
#' @param append
#' @param max.cores
#'
#' @return
#' @export
#'
#' @examples
run_system_parallel <- function(commands, log.basename, append=TRUE, max.cores=8) {
  # make sure commands are single lines
  # no: don't make sure! we might want multiple commands to run sequentially in a single thread
  # commands <- unlist(strsplit(commands, split="\n"))

  # abort if commands is empty
  if (length(commands) == 0)
    return()

  # make sure the target directory for the logs exists, if not create it
  if (!dir.exists(dirname(log.basename))) {
    dir.create(dirname(log.basename), recursive = TRUE)
  }

  commands.filename <- paste0(log.basename, ".commands.sh")
  stdout.filename <- paste0(log.basename, ".stdout.txt")
  stderr.filename <- paste0(log.basename, ".stderr.txt")

  timestamp <- paste("#", date())
  write(timestamp, file = commands.filename, append=append)
  write(commands, file = commands.filename, append=append)

  results <- parallel::mclapply(commands, function(x) {
    # use temporary std.out and std.err
    tmp.stdout <- tempfile(fileext = ".txt")
    tmp.stderr <- tempfile(fileext = ".txt")

    # write the commands to stdout and stderr files prefixed with +
    timestamp <- paste("+", date())
    xsplit <- unlist(strsplit(x, split="\n"))
    write(paste(timestamp, ":", xsplit), tmp.stdout, append=TRUE)
    write(paste(timestamp, ":", xsplit), tmp.stderr, append=TRUE)

    # Run each command in the form:
    # $ (command) 1>> stdout 2>> stderr
    system(paste0("(", x, ")", " 1>> ", tmp.stdout, " 2>> ", tmp.stderr))

    return(list(stdout=tmp.stdout, stderr=tmp.stderr))
  }, mc.cores = max.cores)

  # append the outputs to stdout and stderr
  sapply(results, function(x) {
    system(paste("cat", x$stdout, ">>", stdout.filename))
    system(paste("cat", x$stderr, ">>", stderr.filename))
  })
}




#' Commands chunk
#'
#' Displays commands executed.
#'
#' @param commands
#' @param title
#' @param description
#' @param width
#' @param height
#' @param pdf.filename
#' @param collapsed
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
commands_chunk <- function(commands.filename,
                           stdout.filename = NULL,
                           stderr.filename = NULL,
                           title = "",
                           description = "",
                           collapsed = FALSE,
                           ...) {
  list(
    type = "commands",
    commands.filename = commands.filename,
    stdout.filename = stdout.filename,
    stderr.filename = stderr.filename,
    title = title,
    description = description,
    collapsed = collapsed
  )
}



#' Default function to render a script chunk.
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
render_commands_chunk <- function(chunk, header=TRUE, ...) { #chunk_num, ns, depth=2) {
  num <- gen_id("commands")
  id <- paste0('commands', num)

  chunk$title <- ifelse(is.null(chunk$title), "", chunk$title)
  chunk$description <- ifelse(is.null(chunk$description), "", chunk$description)


  filenames <- c(chunk$commands.filename, chunk$stdout.filename, chunk$stderr.filename)

  tabcontents <- lapply(filenames, function(x) {
    if (file.exists(x)) {
      txt <- readLines(x)
    } else {
      txt <- paste("Error: file", x, "doesn't exist.")
    }

    num_lines <- length(txt)
    pre_class <- if (num_lines > 15) "pre-scrollable" else "pre"

    txt_html <- pre(class=pre_class,
                    style = "white-space: pre; overflow-x: auto; word-wrap: normal;",
                    paste(txt, collapse="\n"))

    paste(txt_html, collapse="\n")
  })

  tabnames <- c("Commands", "Stdout", "Stderr")
  script_html <- HTML(html_nav_tabs(paste0(id, "_tabs"), tabnames, tabcontents))

  # script_html <- HTML(paste(tabcontents, collapse="\n"))

  # generate the figure pdf
  # if (!is.null(chunk$pdf.filename)) {
  #   pdf(file = chunk$pdf.filename, width=chunk$width, height=chunk$height)
  #   chunk$fun()
  #   dev.off()
  #
  #   download_html <- a(href=chunk$pdf.filename, "Download PDF")
  # } else {
  #   download_html <- ""
  # }

  div_class <- if (chunk$collapsed == FALSE) "collapse in" else "collapse"

  if (!is.null(chunk$annotation)) {
    annotation_html <- div(class="panel panel-warning",
                           div(class="panel-heading", "Annotation"),
                           div(class="panel-body", chunk$annotation))

    script_html <- div(class="container-fluid",
                       div(class="row", style="display: flex; align-items: center",
                           div(class="col-xs-8", script_html),
                           div(class="col-xs-4", annotation_html)))
  }

  if (chunk$description != "") {
    label_html <- tags$p(tags$b(paste0("Commands ", num, ": ", chunk$title, " | ")), HTML(chunk$description))
  } else {
    label_html <- tags$p(tags$b(paste0("Commands ", num, ": ", chunk$title)))
  }

  div(class="panel panel-default",
      div(class="panel-body",
          HTML('<table style="width:100%"><tr><td style="text-align: left">'),
          tags$a(href=paste0("#", id), "data-toggle"="collapse", "Show/Hide"),
          HTML('</td><td style="text-align: right">'),
          #download_html,
          HTML('</td></tr></table>'),
          div(id=id, class=div_class, script_html),
          label_html))
}






#' Script chunk
#'
#' A script chunk runs a script.
#'
#' @param commands
#' @param title
#' @param description
#' @param width
#' @param height
#' @param pdf.filename
#' @param collapsed
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
script_chunk <- function(commands,
                         title = "",
                         description = "",
                         collapsed = FALSE,
                         ...) {
  list(
    type = "script",
    commands = commands,
    title = title,
    description = description,
    collapsed = collapsed
  )
}

#' Default function to render a script chunk.
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
render_script_chunk <- function(chunk, header=TRUE, ...) { #chunk_num, ns, depth=2) {
  num <- gen_id("script")
  id <- paste0('script', num)

  chunk$title <- ifelse(is.null(chunk$title), "", chunk$title)
  chunk$description <- ifelse(is.null(chunk$description), "", chunk$description)

  # if (chunk$title != "") {
  #   # TODO: make a css style for the chunk titles instead of using hx
  #   # title_html <- h4(paste0("Figure: ", chunk$title))
  #   title_html <- div(class="h4", paste0("Figure: ", chunk$title))
  # } else {
  #   title_html <- ""
  # }

  # generate the figure png
  # tmp <- tempfile(fileext = ".png")
  #
  # png(tmp, width=chunk$width, height=chunk$height, units = "in", res=96)
  # chunk$fun()
  # dev.off()
  #
  # # if no file was produced, try "plotting" the result of the plooting function (to handle ggplot and others)
  # if (!file.exists(tmp)) {
  #   png(tmp, width=chunk$width, height=chunk$height, units = "in", res=96)
  #   plot(chunk$fun())
  #   dev.off()
  # }
  #
  # # if still no file was produced, show an error
  # if (!file.exists(tmp)) {
  #   figure_html <- div(class="alert alert-danger", "Error: could not make figure.")
  # } else {
  #   # encode into base64
  #   encoded <- openssl::base64_encode(readBin(tmp, "raw", file.info(tmp)[1, "size"]))
  #
  #   figure_html <- img(src=paste0('data:image/png;base64,', encoded))
  # }

  num_lines <- length(strsplit(chunk$commands, split="\n")[[1]])

  pre_class <- if (num_lines > 10) "pre-scrollable" else "pre"
  # pre_class <- "pre"

  script_html <- pre(class=pre_class, style = "white-space: pre; overflow-x: auto; word-wrap: normal;", chunk$commands)

  # generate the figure pdf
  # if (!is.null(chunk$pdf.filename)) {
  #   pdf(file = chunk$pdf.filename, width=chunk$width, height=chunk$height)
  #   chunk$fun()
  #   dev.off()
  #
  #   download_html <- a(href=chunk$pdf.filename, "Download PDF")
  # } else {
  #   download_html <- ""
  # }

  div_class <- if (chunk$collapsed == FALSE) "collapse in" else "collapse"

  if (!is.null(chunk$annotation)) {
    annotation_html <- div(class="panel panel-warning",
                           div(class="panel-heading", "Annotation"),
                           div(class="panel-body", chunk$annotation))

    script_html <- div(class="container-fluid",
                       div(class="row", style="display: flex; align-items: center",
                           div(class="col-xs-8", script_html),
                           div(class="col-xs-4", annotation_html)))
  }

  if (chunk$description != "") {
    label_html <- tags$p(tags$b(paste0("Script ", num, ": ", chunk$title, " | ")), HTML(chunk$description))
  } else {
    label_html <- tags$p(tags$b(paste0("Script ", num, ": ", chunk$title)))
  }

  div(class="panel panel-default",
      div(class="panel-body",
          HTML('<table style="width:100%"><tr><td style="text-align: left">'),
          tags$a(href=paste0("#", id), "data-toggle"="collapse", "Show/Hide"),
          HTML('</td><td style="text-align: right">'),
          #download_html,
          HTML('</td></tr></table>'),
          div(id=id, class=div_class, script_html),
          label_html))
}


