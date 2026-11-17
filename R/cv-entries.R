
library(tibble)

escape_tex <- function(value) {
  replacements <- c(
    "\\" = "\\textbackslash{}",
    "&" = "\\&",
    "%" = "\\%",
    "$" = "\\$",
    "#" = "\\#",
    "_" = "\\_",
    "{" = "\\{",
    "}" = "\\}",
    "~" = "\\textasciitilde{}",
    "^" = "\\textasciicircum{}"
  )

  chars <- strsplit(value, "", fixed = TRUE)[[1]]
  paste0(vapply(chars, function(char) {
    if (char %in% names(replacements)) replacements[[char]] else char
  }, character(1)), collapse = "")
}

escape_html <- function(value) {
  value <- gsub("&", "&amp;", value, fixed = TRUE)
  value <- gsub("<", "&lt;", value, fixed = TRUE)
  value <- gsub(">", "&gt;", value, fixed = TRUE)
  gsub("\"", "&quot;", value, fixed = TRUE)
}

tex_to_html <- function(value) {
  tokens <- character()
  formats <- c(
    textbf = "strong",
    textit = "em",
    emph = "em",
    textsuperscript = "sup",
    textsubscript = "sub"
  )

  for (macro in names(formats)) {
    pattern <- paste0("\\\\", macro, "\\{([^{}]*)\\}")
    matches <- gregexpr(pattern, value, perl = TRUE)
    found <- regmatches(value, matches)[[1]]
    if (length(found) == 0L || identical(found, character(0))) {
      next
    }

    replaced <- vapply(found, function(match) {
      content <- sub(pattern, "\\1", match, perl = TRUE)
      token <- sprintf("CVFMT%04dTOKEN", length(tokens) + 1L)
      tokens[token] <<- paste0(
        "<", formats[[macro]], ">",
        escape_html(content),
        "</", formats[[macro]], ">"
      )
      token
    }, character(1))
    regmatches(value, matches) <- list(replaced)
  }

  value <- gsub("\\&", "&", value, fixed = TRUE)
  value <- gsub("\\%", "%", value, fixed = TRUE)
  value <- gsub("\\_", "_", value, fixed = TRUE)
  value <- gsub("\\\\", "<br>", value, fixed = TRUE)
  value <- escape_html(value)
  for (token in names(tokens)) {
    value <- gsub(token, tokens[[token]], value, fixed = TRUE)
  }
  value
}

cv_entries <- function(data, with, when = NULL, what = NULL,
                       where = NULL, why = NULL, .protect = TRUE) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  n <- nrow(data)
  get_column <- function(column) {
    if (is.null(column)) {
      return(rep("", n))
    }
    if (length(column) != 1L || !is.character(column) ||
        !column %in% names(data)) {
      stop("Entry columns must be names in `data`.", call. = FALSE)
    }

    values <- as.character(data[[column]])
    values[is.na(values)] <- ""
    values
  }
  get_notes <- function(column) {
    if (is.null(column)) {
      return(rep(list(character()), n))
    }
    if (length(column) != 1L || !is.character(column) ||
        !column %in% names(data)) {
      stop("`why` must name a column in `data`.", call. = FALSE)
    }

    values <- data[[column]]
    if (!is.list(values)) {
      values <- as.list(as.character(values))
    }
    lapply(values, function(notes) {
      notes <- as.character(notes)
      notes <- notes[!is.na(notes) & nzchar(notes)]
      notes
    })
  }

  title <- get_column(with)
  date <- get_column(when)
  description <- get_column(what)
  location <- get_column(where)
  notes <- get_notes(why)
  is_latex <- knitr::is_latex_output()
  format_value <- function(value) {
    if (is_latex) {
      if (.protect) escape_tex(value) else value
    } else {
      if (.protect) escape_html(value) else tex_to_html(value)
    }
  }

  if (is_latex) {
    output <- c("\\begin{cventries}")
    for (i in seq_len(n)) {
      note_tex <- if (length(notes[[i]]) == 0L) {
        "{}\\vspace{-4.0mm}"
      } else {
        paste0(
          "{\\begin{cvitems}\n",
          paste0("\\item ", vapply(notes[[i]], format_value, character(1)),
                 collapse = "\n"),
          "\n\\end{cvitems}}"
        )
      }
      output <- c(
        output,
        sprintf(
          "\\cventry{%s}{%s}{%s}{%s}%s",
          format_value(description[i]),
          format_value(title[i]),
          format_value(location[i]),
          format_value(date[i]),
          note_tex
        )
      )
    }
    output <- c(output, "\\end{cventries}")
  } else {
    output <- c('<div class="cv-entries">')
    for (i in seq_len(n)) {
      output <- c(
        output,
        '<div class="cv-entry">',
        sprintf(
          '<div class="cv-entry-main"><strong>%s</strong><span>%s</span></div>',
          format_value(title[i]),
          format_value(location[i])
        ),
        sprintf(
          '<div class="cv-entry-details"><span>%s</span><em>%s</em></div>',
          format_value(description[i]),
          format_value(date[i])
        )
      )
      if (length(notes[[i]]) > 0L) {
        output <- c(
          output,
          '<ul class="cv-entry-notes">',
          paste0("<li>", vapply(notes[[i]], format_value, character(1)), "</li>"),
          "</ul>"
        )
      }
      output <- c(output, "</div>")
    }
    output <- c(output, "</div>")
  }

  cat(paste(output, collapse = "\n"))
  invisible(data)
}
