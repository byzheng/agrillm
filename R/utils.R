#' Format a named list into AI-friendly interface text
#'
#' Converts a structured R list into a readable, bullet-point
#' representation suitable for LLM prompts.
#'
#' @param x A named list.
#' @param indent Indentation level (used internally for recursion).
#' @param bullet Prefix for top-level entries.
#'
#' @return A single character string.
.format_list_for_ai <- function(x, indent = 0, bullet = "- ") {
    stopifnot(is.list(x), !is.null(names(x)))

    pad <- function(n) paste(rep("  ", n), collapse = "")

    lines <- character(0)

    for (nm in names(x)) {
        value <- x[[nm]]

        if (is.list(value) && !is.null(names(value))) {
        # Nested named list
        lines <- c(
            lines,
            paste0(pad(indent), bullet, nm),
            .format_list_for_ai(value, indent = indent + 1, bullet = "")
        )
        } else {
        # Atomic value(s)
        val_str <- if (length(value) == 0 || is.null(value)) {
            "NULL"
        } else if (length(value) == 1) {
            as.character(value)
        } else {
            paste(as.character(value), collapse = ", ")
        }

        lines <- c(
            lines,
            paste0(pad(indent), bullet, nm, ": ", val_str)
        )
        }
    }

    paste(lines, collapse = "\n")
}
