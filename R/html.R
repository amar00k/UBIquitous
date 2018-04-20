

html_panel <- function(body, header=NULL, panel.class="panel-default") {
  div(class=paste("panel", panel.class),
      div(class="panel-body", body))
}

html_nav_tabs <- function(id, tabnames, tabcontents) {
  ntabs <- length(tabnames)

  # tab buttons
  li <- paste0('<li', c(' class="active"', rep("", ntabs-1)), '>',
               '<a data-toggle="tab" href="#', id, "_", 1:ntabs ,'">', tabnames,'</a></li>',
               collapse="\n")
  li <- paste('<ul class="nav nav-tabs">', li, '</ul>', sep="\n")

  content <- paste0('<div id="', id, "_", 1:ntabs,'" class="tab-pane fade ', c('in active', rep("", ntabs-1)), '">',
                    tabcontents,
                    '</div>',
                    collapse="\n")

  content <- paste('<div class="tab-content">', content, '</div>', sep="\n")

  paste(li, content, sep="\n")
}

