# Variable, global to package's namespace.
# This function is not exported to user space and does not need to be documented.
AGRILLM_OPTIONS <- settings::options_manager(
    chatgpt = list(
        apikey = ""
    ),
    default = "chatgpt"
)


#' Set or get options for agrillm
#'
#' This function allows users to get or set configuration options for the
#' agrillm package.
#'
#' @param ... Option names to retrieve or key-value pairs to set.
#'
#' @details
#' The options are managed via a nested structure that distinguishes between
#' the agrillm package.
#'
#' @section Supported options:
#' \describe{
#'   \item{\code{chatgpt.apikey}}{ API key for ChatGPT integration. }
#' }
#'
#' @return If called with no arguments, returns all current options.
#' If called with named arguments, updates and returns the modified options.
#'
#' @examples
#' # Get all options
#' agrillm_options()
#'
#' # Set apikey
#' agrillm_options(chatgpt.apikey = "your_api_key_here")
#'
#' @export
agrillm_options <- function(...) {
    args <- list(...)
    if (length(args) == 0) {
        return(AGRILLM_OPTIONS())
    }

    if (is.null(names(args)) || any(nchar(names(args)) == 0)) {
        stop("All arguments must be named.", call. = FALSE)
    }

    for (key in names(args)) {
        value <- args[[key]]

        if (grepl("\\.", key)) {
            parts <- strsplit(key, "\\.")[[1]]
            if (length(parts) != 2) {
                stop(sprintf("Invalid option name: '%s'", key), call. = FALSE)
            }
            
            group <- parts[1]
            subkey <- parts[2]

            if (!group %in% c("chatgpt")) {
                stop(sprintf("Unknown group: '%s' (must be 'chatgpt')", group), call. = FALSE)
            }

            # Get current value to check type
            current_value <- AGRILLM_OPTIONS()[[group]][[subkey]]
            .check_option_value(current_value, value)

            current <- AGRILLM_OPTIONS()[[group]]
            if (is.null(current)) {
                current <- list()
            }
            current[[subkey]] <- value
            
            # Update the entire group
            update_args <- list()
            update_args[[group]] <- current
            do.call(AGRILLM_OPTIONS, update_args)
        } else {
            # Handle top-level options
            if (!key %in% c("default")) {
                stop(sprintf("Unknown top-level option: '%s' (must be 'default')", key), call. = FALSE)
            }
            
            # Get current value to check type
            current_value <- AGRILLM_OPTIONS()[[key]]
            .check_option_value(current_value, value)
            
            # Update the top-level option
            update_args <- list()
            update_args[[key]] <- value
            do.call(AGRILLM_OPTIONS, update_args)
        }
    }
    
    invisible(AGRILLM_OPTIONS())
}

#' Reset all agrillm options to defaults
#'
#' This restores all package settings to their initial defaults as defined
#' when agrillm was loaded. Use this if you want to undo all customizations.
#'
#' @return Invisibly returns the default settings after reset.
#' @examples
#' agrillm_options(chatgpt.apikey = "your_api_key_here")
#' agrillm_reset()
#' agrillm_options()
#' @export
agrillm_reset <- function() {
    settings::reset(AGRILLM_OPTIONS)
    invisible(AGRILLM_OPTIONS())
}


#' Get agrillm package option
#'
#' Retrieves the value of a specified option from the agrillm package configuration.
#' This function provides access to package-level settings and preferences.
#'
#' @param name A character string specifying the name of the option to retrieve.
#'
#' @return The value of the specified option, or NULL if the option does not exist.
#'
#' @examples
#' # Get the default apikey
#' get_agrillm_option("chatgpt.apikey")
#'
#' @export
get_agrillm_option <- function(name) {
    parts <- strsplit(name, "\\.")[[1]]
    if (length(parts) == 1) {
        AGRILLM_OPTIONS()[[parts]]
    } else {
        AGRILLM_OPTIONS()[[parts[1]]][[parts[2]]]
    }
}


.check_option_value <- function(current_value, value) {
    if (is.null(value)) {
        stop("Option value cannot be NULL", call. = FALSE)
    }
    if (class(value) != class(current_value)) {
        stop(sprintf("Option expects %s, got %s", class(current_value), class(value)), call. = FALSE)
    }
    if (length(current_value) != length(value)) {
        stop(sprintf("Option expects length %d, got length %d", length(current_value), length(value)), call. = FALSE)
    }
}
