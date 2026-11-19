cv_html_header <- function(cv) {
  required <- c(
    "address", "pronouns", "email", "homepage", "github", "orcid", "bluesky",
    "photo"
  )
  if (!is.list(cv) || !all(required %in% names(cv))) {
    stop("`cv` must contain all header fields.", call. = FALSE)
  }

  icon <- function(name) as.character(fontawesome::fa(name))
  item <- function(name, label, href = NULL) {
    content <- paste0(
      icon(name),
      "<span>", htmltools::htmlEscape(label), "</span>"
    )
    if (!is.null(href)) {
      content <- paste0(
        '<a href="', htmltools::htmlEscape(href, attribute = TRUE),
        '">', content, "</a>"
      )
    }

    paste0('<div class="cv-profile-item">', content, "</div>")
  }

  items <- c(
    item("location-dot", cv$address),
    item("comment", cv$pronouns),
    item("envelope", cv$email, paste0("mailto:", cv$email)),
    item("house", cv$homepage, paste0("https://", cv$homepage)),
    item("github", cv$github, paste0("https://github.com/", cv$github)),
    item("orcid", cv$orcid, paste0("https://orcid.org/", cv$orcid)),
    item(
      "bluesky",
      cv$bluesky,
      paste0("https://bsky.app/profile/", cv$bluesky)
    )
  )

  cat(
    '<div class="cv-profile">',
    '<img src="', htmltools::htmlEscape(cv$photo, attribute = TRUE),
    '" alt="Profile image">',
    '<div class="cv-profile-info">',
    paste(items, collapse = ""),
    "</div></div>",
    sep = "\n"
  )
}

cv_section_icon <- function(name) {
  if (knitr::is_latex_output()) {
    sprintf("\\faIcon{%s}", name)
  } else {
    as.character(fontawesome::fa(name))
  }
}
