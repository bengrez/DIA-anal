html_escape <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&#39;", x, fixed = TRUE)
  x
}

df_to_html_table <- function(df, max_rows = 200) {
  if (is.null(df) || nrow(df) == 0) {
    return("<p><em>Sin tabla para mostrar.</em></p>")
  }
  df2 <- df
  if (nrow(df2) > max_rows) {
    df2 <- utils::head(df2, max_rows)
  }

  th <- paste0("<th>", html_escape(names(df2)), "</th>", collapse = "")
  rows <- apply(df2, 1, function(r) paste0("<tr>", paste0("<td>", html_escape(r), "</td>", collapse = ""), "</tr>"))
  paste0(
    "<table border='1' cellspacing='0' cellpadding='6' style='border-collapse:collapse; font-family: Arial, sans-serif; font-size: 12px;'>",
    "<thead><tr>", th, "</tr></thead>",
    "<tbody>", paste(rows, collapse = ""), "</tbody>",
    "</table>"
  )
}

write_text_file <- function(path, text) {
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(enc2utf8(text)), con)
}

