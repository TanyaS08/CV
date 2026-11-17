document <- readLines("index.qmd", warn = FALSE)
front_matter_start <- which(document == "---")[1]
front_matter_end <- which(document == "---" & seq_along(document) > front_matter_start)[1]
metadata <- yaml::yaml.load(
  paste(document[(front_matter_start + 1):(front_matter_end - 1)], collapse = "\n")
)
cv <- metadata$params$cv$value

required <- c(
  "first_name", "last_name", "position", "address", "photo", "pronouns",
  "email", "homepage", "github", "orcid", "bluesky", "accent_color",
  "footer_date_format"
)
if (!is.list(cv) || !all(required %in% names(cv))) {
  stop("The `params.cv.value` front matter must define all CV header fields.")
}

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
  chars <- strsplit(as.character(value), "", fixed = TRUE)[[1]]
  paste0(vapply(chars, function(char) {
    if (char %in% names(replacements)) replacements[[char]] else char
  }, character(1)), collapse = "")
}

commands <- c(
  sprintf("\\name{%s}{%s}", escape_tex(cv$first_name), escape_tex(cv$last_name)),
  sprintf("\\position{%s}", escape_tex(cv$position)),
  sprintf("\\address{%s}", escape_tex(cv$address)),
  sprintf("\\photo{%s}", escape_tex(cv$photo)),
  sprintf("\\pronouns{%s}", escape_tex(cv$pronouns)),
  sprintf("\\email{%s}", escape_tex(cv$email)),
  sprintf("\\homepage{%s}", escape_tex(cv$homepage)),
  sprintf("\\github{%s}", escape_tex(cv$github)),
  sprintf("\\orcid{%s}", escape_tex(cv$orcid)),
  sprintf("\\bluesky{%s}", escape_tex(cv$bluesky)),
  sprintf(
    "\\makecvfooter{%s}{%s}{\\thepage\\ of \\pageref{LastPage}}",
    escape_tex(format(Sys.Date(), cv$footer_date_format)),
    escape_tex(paste(cv$first_name, cv$last_name))
  )
)

accent_color <- as.character(cv$accent_color)
if (!grepl("^#?[[:xdigit:]]{6}$", accent_color)) {
  stop("`params.cv.value.accent_color` must be a six-digit hex color.")
}
accent_hex <- toupper(sub("^#", "", accent_color))

writeLines(
  c(
    sprintf("\\definecolor{cvaccent}{HTML}{%s}", accent_hex),
    "\\colorlet{awesome}{cvaccent}",
    "\\AtBeginDocument{",
    paste0("  ", commands),
    "}"
  ),
  "includes/generated-cv-header.tex"
)
writeLines(
  c(":root {", sprintf("  --cv-accent: #%s;", accent_hex), "}"),
  "includes/generated-cv-theme.css"
)

if (!requireNamespace("rsvg", quietly = TRUE)) {
  stop("The `rsvg` R package is required to render the Bluesky icon in PDF.")
}
rsvg::rsvg_pdf(
  "assets/icon/bluesky.svg",
  file = "includes/generated-bluesky.pdf"
)
