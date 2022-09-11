library(htmltools)
library(stringr)
library(dplyr)

# Generates <i class="icon"></i>
make_icon_img <- function(icon) {
  return(htmltools::tag("img", list(src = icon)))
}

# make_icon_text <- function(icon, text) {
#   return(htmltools::HTML(paste0(make_icon(icon), " ", text)))
# }

icon_link_img <- function(icon = NULL, url = NULL) {
  if (!is.null(icon)) {
    fig <- make_icon_img(icon)
  }
  return(htmltools::a(href = url, fig, class = "icon-link"))
}

