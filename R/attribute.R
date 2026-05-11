
#' Add custom attributes to APSIMX object
#' 
#' This function allows adding custom attributes to the APSIMX object, which can be used to store additional metadata or assumptions related to the decision session. It ensures that all relevant information is encapsulated within the APSIMX object for easy access during later stages of the workflow.
#' 
#' @param apsimx APSIM model object as returned by `rapsimng::read_apsimx()`.
#' @param attribute_name Character scalar name of the attribute to add.
#' @param attribute_value List containing the attribute values to add.
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
        agrillm[[attribute_name]] <- utils::modifyList(
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

#' Get custom attributes from APSIMX object
#' 
#' @description 
#' This function retrieves custom attributes from the APSIMX object that were added using the `.add_attribute()` function. It allows access to metadata or assumptions stored within the APSIMX object, which can be useful for summarising assumptions in reports or for debugging purposes.
#' 
#' @param apsimx APSIM model object as returned by `rapsimng::read_apsimx()`.
#' @param attribute_name Character scalar name of the attribute to retrieve. If NULL, returns all attributes under "agrillm".
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

