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


## Generates citations publication page

get_pubs <- function() {
  pubs <- read.csv("files/pub_list.csv")
  pubs <- make_citations(pubs)
  pubs$stub <- make_stubs(pubs)
  pubs$url_details <- file.path('research', pubs$stub, "index.html")
  return(pubs)
}

make_citations <- function(pubs) {
  pubs$citation <- unlist(lapply(split(pubs, 1:nrow(pubs)), make_citation))
  return(pubs)
}

make_citation <- function(pub) {
  if (!is.na(pub$journal)) {
    pub$journal <- paste0('_', pub$journal, '_.')
  }
  if (!is.na(pub$number)) {
    pub$number <- paste0(pub$number, '.')
  }
  if (!is.na(pub$doi)) {
    pub$doi <- make_doi(pub$doi)
  }
  pub$year <- paste0("(", pub$year, ")")
  pub$title <- paste0('"', pub$title, '"')
  pub[,which(is.na(pub))] <- ''
  paste(
    pub$author, pub$year, pub$title, pub$journal, 
    pub$number, pub$doi)
}

make_doi <- function(doi) {
  return(paste0('DOI: [', doi, '](', 'https://doi.org/', doi, ')'))
}

make_stubs <- function(pubs) {
  journal <- str_to_lower(pubs$journal)
  journal <- str_replace_all(journal, ':', '')
  journal <- str_replace_all(journal, '`', '')
  journal <- str_replace_all(journal, "'", '')
  journal <- str_replace_all(journal, "\\.", '')
  journal <- str_replace_all(journal, "&", '')
  journal <- str_replace_all(journal, ',', '')
  journal <- str_replace_all(journal, '  ', '-')
  journal <- str_replace_all(journal, ' ', '-')
  return(paste0(pubs$year, '-', journal))
}

make_pub_list <- function(pubs, category) {
  x <- pubs[which(pubs$category == category),]
  pub_list <- lapply(split(x, 1:nrow(x)), make_pub)
  return(paste(unlist(pub_list), collapse = ""))
}

make_pub <- function(pub) {
  index <- parent.frame()$i[] # index number from the lapply
  header <- FALSE
  if (index == 1) { header <- TRUE }
  return(paste0('<div class="pub">', as.character(markdown_to_html(text = paste0(index, ') ', pub$citation))), make_icons(pub), '</div>'))
}

make_icons <- function(pub) {
  html <- c()
  if (!is.na(pub$url_pub)) {
    html <- c(html, as.character(icon_link_custom(
      text = "Link to paper",
      url  = pub$url_pub
    )))
  }
  return(paste(html, collapse = ""))
}

icon_link_custom <- function(
  text = NULL,
  url = NULL,
  class = "icon-link",
  target = "_blank"
) {
  if (!is.null(text)) {
    text <- make_icon_text(text)
  }
  return(htmltools::a(
    href = url, text, class = class, target = target, rel = "noopener"
  ))
}

markdown_to_html <- function(text) {
  if (is.null(text)) { return(text) }
  return(HTML(markdown::renderMarkdown(text = text)))
}
