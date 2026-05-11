#' Distribute Total Soil Mineral Nitrogen Across Soil Layers
#'
#' Allocates a total profile mineral nitrogen amount (\eqn{kg/ha})
#' into soil layers using a generic depth-weighted kernel.
#' This function is deterministic, returns non-negative layer values,
#' and conserves the supplied total nitrogen.
#'
#' The exponential method is intended as a pragmatic default for
#' pre-sowing initialisation when measured soil nitrate profiles
#' are unavailable, representing situations where mineralised
#' nitrogen from decomposing residues and roots is concentrated
#' near the soil surface and declines with depth. Alternative
#' kernels are provided to support simpler assumptions or
#' truncated rooting zones.
#'
#' @param total_n Numeric scalar. Total profile mineral nitrogen to
#'   distribute (\eqn{kg/ha}). Must be finite and non-negative.
#' @param thickness_mm Numeric vector. Layer thicknesses in mm.
#'   All values must be finite and strictly positive.
#' @param method Character scalar. Distribution kernel to use:
#'   `"exponential"` (default), `"uniform"`, or `"truncated"`.
#' @param lambda_mm Numeric scalar. Exponential decay length scale in mm,
#'   used only when `method = "exponential"`. Smaller values produce
#'   steeper surface concentration; larger values approach uniform
#'   distributions.
#' @param max_depth_mm Numeric scalar. Cutoff depth in mm for
#'   `method = "truncated"`; nitrogen is distributed uniformly from
#'   0 to this depth and zero below.
#'
#' @return Numeric vector of layer nitrogen values (\eqn{kg/ha}),
#'   with length equal to `length(thickness_mm)`.
#'
#' @details
#' This function does not attempt to reproduce measured soil nitrate
#' profiles or rainfall-driven redistribution. Where detailed
#' soil nitrogen measurements or continuous system spin-up
#' simulations are available, those approaches should be preferred.
#'
#' @export
#'
#' @examples
#' library(agrillm)
#' thickness <- rep(100, 10)
#'
#' distribute_soil_n(50, thickness)
#' distribute_soil_n(50, thickness, method = "uniform")
#' distribute_soil_n(50, thickness, method = "truncated", max_depth_mm = 600)
#'
#' # Compare distribution methods in a single figure.
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   layer_bounds <- cumsum(thickness)
#'   layer_labels <- paste0(c(0, head(layer_bounds, -1)), "-", layer_bounds, " mm")
#'
#'   profiles <- rbind(
#'     data.frame(layer = layer_labels,
#'                n_kg_ha = distribute_soil_n(50, thickness, method = "uniform"),
#'                method = "uniform"),
#'     data.frame(layer = layer_labels,
#'                n_kg_ha = distribute_soil_n(50, thickness, method = "exponential", lambda_mm = 500),
#'                method = "exponential"),
#'     data.frame(layer = layer_labels,
#'                n_kg_ha = distribute_soil_n(50, thickness, method = "truncated", max_depth_mm = 600),
#'                method = "truncated")
#'   ) |>
#'   dplyr::mutate(layer = factor(layer, levels = layer_labels))
#'   profiles$method <- factor(profiles$method, levels = c("uniform", "exponential", "truncated"))
#'
#'   ggplot2::ggplot(profiles, ggplot2::aes(x = layer, y = n_kg_ha, fill = method)) +
#'     ggplot2::geom_col(position = "dodge") +
#'     ggplot2::labs(
#'       title = "Example Soil N Allocation by Method",
#'       x = "Soil layer",
#'       y = "N per layer (kg/ha)",
#'       fill = "Method"
#'     ) +
#'     ggplot2::theme_minimal(base_size = 11) +
#'     ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
#' }
#'
distribute_soil_n <- function(
    total_n,
    thickness_mm,
    method = c("exponential", "uniform", "truncated"),
    lambda_mm = 500,
    max_depth_mm = NULL
) {
    if (!is.numeric(total_n) || length(total_n) != 1 || !is.finite(total_n) || total_n < 0) {
        stop("total_n must be a single finite non-negative number.")
    }
    if (!is.numeric(thickness_mm) || length(thickness_mm) == 0 || any(!is.finite(thickness_mm)) || any(thickness_mm <= 0)) {
        stop("thickness_mm must be a non-empty numeric vector of finite positive values")
    }

    method <- match.arg(method)

    if (method == "exponential") {
        if (!is.numeric(lambda_mm) || length(lambda_mm) != 1 || !is.finite(lambda_mm) || lambda_mm <= 0) {
            stop("lambda_mm must be a single finite positive number for exponential method")
        }
    }

    if (method == "truncated") {
        if (is.null(max_depth_mm) || !is.numeric(max_depth_mm) || length(max_depth_mm) != 1 || !is.finite(max_depth_mm) || max_depth_mm <= 0) {
            stop("max_depth_mm must be supplied as a single finite positive number for truncated method")
        }
    }

    if (total_n == 0) {
        return(rep(0, length(thickness_mm)))
    }

    depth_top <- c(0, cumsum(thickness_mm)[-length(thickness_mm)])
    depth_bottom <- cumsum(thickness_mm)

    weights <- switch(
        method,
        uniform = thickness_mm,
        exponential = exp(-depth_top / lambda_mm) - exp(-depth_bottom / lambda_mm),
        truncated = pmax(0, pmin(depth_bottom, max_depth_mm) - pmin(depth_top, max_depth_mm))
    )

    if (any(weights < 0) || sum(weights) <= 0) {
        stop("Invalid distribution weights computed from inputs")
    }

    n_per_layer <- total_n * weights / sum(weights)
    n_per_layer[n_per_layer < 0] <- 0

    # Ensure exact conservation after floating-point operations.
    remainder <- total_n - sum(n_per_layer)
    if (remainder != 0) {
        n_per_layer[which.max(weights)] <- n_per_layer[which.max(weights)] + remainder
    }

    n_per_layer
}


