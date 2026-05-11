
#' ## Add custom attributes to APSIMX object
#' 
#' This function allows adding custom attributes to the APSIMX object, which can be used to store additional metadata or assumptions related to the decision session. It ensures that all relevant information is encapsulated within the APSIMX object for easy access during later stages of the workflow.
.add_attribute <- function(apsimx, attribute_name, attribute_value) {
    stopifnot(inherits(apsimx, "list"))
    stopifnot(is.character(attribute_name), length(attribute_name) == 1)
    stopifnot(is.list(attribute_value))

    # initialise agrillm attribute if missing
    agrillm <- attr(apsimx, "agrillm")
    if (is.null(agrillm)) {
        agrillm <- list()
    }

    # merge with existing attribute if present
    if (!is.null(agrillm[[attribute_name]])) {
        agrillm[[attribute_name]] <- modifyList(
            agrillm[[attribute_name]],
            attribute_value,
            keep.null = TRUE
        )
    } else {
        agrillm[[attribute_name]] <- attribute_value
    }

    attr(apsimx, "agrillm") <- agrillm
    apsimx
}

#' ## Get custom attributes from APSIMX object
#' 
#' This function retrieves custom attributes from the APSIMX object that were added using the `.add_attribute()` function. It allows access to metadata or assumptions stored within the APSIMX object, which can be useful for summarising assumptions in reports or for debugging purposes.
.get_attribute <- function(apsimx, attribute_name = NULL) {
    stopifnot(inherits(apsimx, "list"))
    agrillm <- attr(apsimx, "agrillm")
    if (is.null(agrillm)) {
        return(NULL)
    }
    if (is.null(attribute_name)) {
        return(agrillm)
    }
    stopifnot(is.character(attribute_name), length(attribute_name) == 1)
    return(agrillm[[attribute_name]])
}

