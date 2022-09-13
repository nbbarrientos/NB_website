library(htmltools)
library(distilltools)
library(stringr)
library(dplyr)

# Generates <i class="icon"></i>
make_icon_img <- function(icon) {
  return(htmltools::tag("img", list(src = icon)))
}

icon_link_img <- function(icon = NULL, url = NULL) {
  if (!is.null(icon)) {
    fig <- make_icon_img(icon)
  }
  return(htmltools::a(href = url, fig, class = "icon-link"))
}

make_icon_text <- function(text) {
   return(htmltools::HTML(paste0(text)))
}

icon_link_text <- function(text = NULL, url = NULL) {
  if (!is.null(text)) {
    fig <- make_icon_text(text)
  }
  return(htmltools::a(href = url, fig, class = "icon-link"))
}