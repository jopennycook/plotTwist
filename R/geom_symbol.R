#' Title
#'
#' @param mapping
#' @param data
#' @param stat
#' @param position
#' @param ...
#' @param na.rm
#' @param show.legend
#' @param inherit.aes
#'
#' @return
#' @import ggplot2
#' @import grid
#' @import rlang
#' @export
#'
#' @examples
geom_symbol <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ...,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
        layer(
                data = data,
                mapping = mapping,
                stat = stat,
                geom = GeomSymbol,
                position = position,
                show.legend = show.legend,
                inherit.aes = inherit.aes,
                params = list(
                        na.rm = na.rm,
                        ...
                )
        )
}

draw_key_symbol <- function(data, params, size) {
        symbol_recipe <- symbol_recipes[[data$symbol]]
        if (head(symbol_recipe$x, 1) == tail(symbol_recipe$x, 1) &
            head(symbol_recipe$y, 1) == tail(symbol_recipe$y, 1)) {
                geom_output <- polygonGrob(x = unit(0.5, "npc") + unit(symbol_recipe$x * data$size, "mm"),
                                           y = unit(0.5, "npc") + unit(symbol_recipe$y * data$size, "mm"),
                                           gp = gpar(col = alpha(data$colour %||% "black", data$alpha),
                                                     fill = alpha(data$fill %||% "black", data$alpha),
                                                     lwd = data$stroke %||% 0.5))
        } else {
                geom_output <- polylineGrob(x = unit(0.5, "npc") + unit(symbol_recipe$x * data$size, "mm"),
                                            y = unit(0.5, "npc") + unit(symbol_recipe$y * data$size, "mm"),
                                            id = symbol_recipe$poly,
                                            gp = gpar(col = alpha(data$colour %||% "black", data$alpha),
                                                      lwd = data$stroke %||% 0.5))
        }
        geom_output
}

GeomSymbol <- ggplot2::ggproto("GeomSymbol", Geom,
                      required_aes = c("x", "y"),
                      non_missing_aes = c("size", "symbol", "colour"),
                      default_aes = aes(
                              symbol = 1, colour = "black", size = 1, fill = NA,
                              alpha = NA, stroke = 0.5),

                      # the function to draw symbols placed according to 'x', 'y', and 'symbol' aesthetics
                      draw_panel = function(self, data, panel_params, coord, na.rm = FALSE) {
                              coords <- coord$transform(data, panel_params)

                              # a list of grobs is created with one symbol for each data point
                              grob_list <- lapply(seq(nrow(coords)), function(i_symbol) {

                                      # the coordinates for the correct symbol are loaded from symbol_recipe
                                      symbol_recipe <- symbol_recipes[[coords$symbol[i_symbol]]]

                                      if (head(symbol_recipe$x, 1) == tail(symbol_recipe$x, 1) &
                                          head(symbol_recipe$y, 1) == tail(symbol_recipe$y, 1)) {

                                              # if the path begins and ends in the same place,
                                              # a solid polygon with an outline and fill parameter is created
                                              grob_output <- polygonGrob(x = unit(coords$x[i_symbol], "native") +
                                                                                 unit(symbol_recipe$x * coords$size[i_symbol], "mm"),
                                                                         y = unit(coords$y[i_symbol], "native") +
                                                                                 unit(symbol_recipe$y * coords$size[i_symbol], "mm"),
                                                                         gp = gpar(col = alpha(coords$colour[i_symbol], coords$alpha[i_symbol]),
                                                                                   fill = alpha(coords$fill[i_symbol], coords$alpha[i_symbol]),
                                                                                   lwd = coords$stroke[i_symbol]))
                                      } else {

                                              # if the outline does not begin and end in the same place
                                              # a line is created with an outline but no fill parameter
                                              grob_output <- polylineGrob(x = unit(coords$x[i_symbol], "native") +
                                                                                  unit(symbol_recipe$x * coords$size[i_symbol], "mm"),
                                                                          y = unit(coords$y[i_symbol], "native") +
                                                                                  unit(symbol_recipe$y * coords$size[i_symbol], "mm"),
                                                                          id = symbol_recipe$poly,
                                                                          gp = gpar(col = alpha(coords$colour[i_symbol], coords$alpha[i_symbol]),
                                                                                    lwd = coords$stroke[i_symbol]))
                                      }

                                      # output each individual data point symbol
                                      grob_output
                              })

                              # collect all the symbols into a single grob with grobTree
                              do.call(grobTree, grob_list)
                      },

                      # function to draw correct symbol in legend key
                      draw_key = draw_key_symbol
)

#' Title
#'
#' @param ...
#' @param values
#' @param breaks
#' @param na.value
#'
#' @return
#' @export
#'
#' @examples
scale_symbol_manual <- function (..., values, breaks = waiver(), na.value = NA) {
        manual_scale("symbol", values, breaks, ..., na.value = na.value)
}

symbol_recipes <- list(
        "triangle" = list("x" = c(-0.874, 0.875, 0, -0.874),
                          "y" = c(-0.513, -0.513, 1, -0.513)),
        "downward_triangle" = list("x" = c(-0.874, 0.875, 0, -0.874),
                                   "y" = c(0.513, 0.513, -1, 0.513)),
        "square" = list("x" = c(-0.707, -0.707, 0.707, 0.707, -0.707),
                        "y" = c(0.707, -0.707, -0.707, 0.707, 0.707)),
        "diamond" = list("x" = c(-1, 0, 1, 0, -1),
                         "y" = c(0, -1, 0, 1, 0)),
        "pentagon" = list("x" = c(0, 0.952, 0.589, -0.589, -0.952, 0),
                          "y" = c(1, 0.309, -0.810, -0.810, 0.309, 1)),
        "hexagon" = list("x" = c(0, 0.875, 0.875, 0, -0.875, -0.875, 0),
                         "y" = c(1, 0.510, -0.510, -1, -0.510, 0.510, 1)),
        "septagon" = list("x" = c(0, -0.779, -0.971, -0.432, 0.432, 0.971,
                                  0.779, 0),
                          "y" = c(-1, -0.625, 0.217, 0.893, 0.893, 0.217,
                                  -0.625, -1)),
        "octagon" = list("x" = c(0, 0.707, 1, 0.707, 0, -0.707, -1, -0.707, 0),
                         "y" = c(1, 0.707, 0, -0.707, -1, -0.707, 0, 0.707, 1)),
        "hollow_triangle" = list("x" = c(-0.874, 0.875, 0, -0.874, 0.875),
                                 "y" = c(-0.513, -0.513, 1, -0.513, -0.513)),
        "hollow_downward_triangle" = list("x" = c(-0.874, 0.875, 0, -0.874,
                                                  0.875),
                                          "y" = c(0.513, 0.513, -1, 0.513,
                                                  0.513)),
        "hollow_square" = list("x" = c(-0.707, -0.707, 0.707, 0.707, -0.707,
                                       -0.707),
                               "y" = c(0.707, -0.707, -0.707, 0.707, 0.707,
                                       -0.707)),
        "hollow_diamond" = list("x" = c(-1, 0, 1, 0, -1, 0),
                                "y" = c(0, -1, 0, 1, 0, -1)),
        "hollow_pentagon" = list("x" = c(0, 0.952, 0.589, -0.589, -0.952, 0,
                                         0.952),
                                 "y" = c(1, 0.309, -0.810, -0.810, 0.309, 1,
                                         0.309)),
        "hollow_hexagon" = list("x" = c(0, 0.875, 0.875, 0, -0.875, -0.875, 0,
                                        0.875),
                                "y" = c(1, 0.510, -0.510, -1, -0.510, 0.510, 1,
                                        0.510)),
        "hollow_septagon" = list("x" = c(0, -0.779, -0.971, -0.432, 0.432,
                                         0.971, 0.779, 0, -0.779),
                                 "y" = c(-1, -0.625, 0.217, 0.893, 0.893,
                                         0.217, -0.625, -1, -0.625)),
        "hollow_octagon" = list("x" = c(0, 0.707, 1, 0.707, 0, -0.707, -1,
                                        -0.707, 0, 0.707),
                                "y" = c(1, 0.707, 0, -0.707, -1, -0.707, 0,
                                        0.707, 1, 0.707)),
        "triangle_cross" = list("x" = c(-0.874, 0, 0, 0, 0.874, 0),
                                "y" = c(-0.513, 0, 1, 0, -0.513, 0),
                                "poly" = c(1, 1, 2, 2, 3, 3)),
        "downward_triangle_cross" = list("x" = c(-0.874, 0, 0, 0, 0.874, 0),
                                         "y" = c(0.513, 0, -1, 0, 0.513, 0),
                                         "poly" = c(1, 1, 2, 2, 3, 3)),
        "square_cross" = list("x" = c(-0.707, 0.707, -0.707, 0.707),
                              "y" = c(-0.707, 0.707, 0.707, -0.707),
                              "poly" = c(1, 1, 2, 2)),
        "diamond_cross" = list("x" = c(-1, 1, 0, 0),
                               "y" = c(0, 0, 1, -1),
                               "poly" = c(1, 1, 2, 2)),
        "pentagon_cross" = list("x" = c(0, 0, 0, 0.952, 0, 0.589, 0, -0.589,
                                        0, -0.952),
                                "y" = c(0, 1, 0, 0.309, 0, -0.810, 0, -0.810,
                                        0, 0.309),
                                "poly" = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)),
        "hexagon_cross" = list("x" = c(0, 0, 0, 0.875, 0, 0.875, 0, 0, 0,
                                       -0.875, 0, -0.875),
                               "y" = c(0, 1, 0, 0.510, 0, -0.510, 0, -1, 0,
                                       -0.510, 0, 0.510),
                               "poly" = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6)),
        "septagon_cross" = list("x" = c(0, 0, 0, -0.779, 0, -0.971, 0, -0.432,
                                        0, 0.432, 0, 0.971, 0, 0.779),
                                "y" = c(0, -1, 0, -0.625, 0, 0.217, 0, 0.893,
                                        0, 0.893, 0, 0.217, 0, -0.625),
                                "poly" = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
                                           7, 7)),
        "octagon_cross" = list("x" = c(0, 0, 0, 0.707, 0, 1, 0, 0.707, 0, 0,
                                       0, -0.707, 0, -1, 0, -0.707),
                               "y" = c(0, 1, 0, 0.707, 0, 0, 0, -0.707, 0, -1,
                                       0, -0.707, 0, 0, 0, 0.707),
                               "poly" = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
                                          7, 7, 8, 8)),
        "triangle_zigzag" = list("x" = c(-0.874, 0, 0.874),
                                 "y" = c(-0.513, 1, -0.513)),
        "downward_triangle_zigzag" = list("x" = c(-0.874, 0, 0.874),
                                          "y" = c(0.513, -1, 0.513)),
        "square_zigzag" = list("x" = c(-0.707, 0.707, -0.707, 0.707),
                               "y" = c(-0.707, -0.707, 0.707, 0.707)),
        "diamond_zigzag" = list("x" = c(0, -1, 1, 0),
                                "y" = c(1, 0, 0, -1)),
        "pentagon_zigzag" = list("x" = c(-0.952, -0.589, 0, 0.589, 0.952),
                                 "y" = c(0.309, -0.810, 1, -0.810, 0.309)),
        "hexagon_zigzag" = list("x" = c(0, 0.875, -0.875, 0.875, -0.875, 0),
                                "y" = c(1, 0.510, 0.510, -0.510, -0.510, -1)),
        "septagon_zigzag" = list("x" = c(-0.971, -0.779, -0.432, 0, 0.432,
                                         0.779, 0.971),
                                 "y" = c(0.217, -0.625, 0.893, -1, 0.893,
                                         -0.625, 0.217)),
        "octagon_zigzag" = list("x" = c(0, -0.707, 0.707, -1, 1, -0.707,
                                        0.707, 0),
                                "y" = c(1, 0.707, 0.707, 0, 0, -0.707,
                                        -0.707, -1)),
        "triangle_squiggle" = list("x" = c(0, 0.437, -0.874, 0.874),
                                   "y" = c(1, 0.245, -0.513, -0.513)),
        "downward_triangle_squiggle" = list("x" = c(0.874, -0.874, 0.437, 0),
                                            "y" = c(0.513, 0.513, -0.245, -1)),
        "square_squiggle" = list("x" = c(-0.707, -0.707, 0.707, 0.707),
                                 "y" = c(0.707, -0.707, 0.707, -0.707)),
        "diamond_squiggle" = list("x" = c(-1, 0, 0, 1),
                                  "y" = c(0, -1, 1, 0)),
        "pentagon_squiggle" = list("x" = c(-0.589, -0.952, -0.476, 0.589,
                                           0.952, 0),
                                   "y" = c(-0.810, 0.309, 0.655, -0.810,
                                           0.309, 1)),
        "hexagon_squiggle" = list("x" = c(-0.875, -0.875, 0, 0, 0.875, 0.875),
                                  "y" = c(0.510, -0.510, -1, 1, 0.510, -0.510)),
        "septagon_squiggle" = list("x" = c(-0.971, -0.432, 0.432, 0.701,
                                           -0.779, 0, 0.779, 0.971),
                                   "y" = c(0.217, 0.893, 0.893, 0.555,
                                           -0.625, -1, -0.625, 0.217)),
        "octagon_squiggle" = list("x" = c(0.707, 0, -0.707, -1, 1, 0.707, 0,
                                          -0.707),
                                  "y" = c(0.707, 1, 0.707, 0, 0, -0.707, -1,
                                          -0.707)),
        "forward_slash" = list("x" = c(-0.707, 0.707),
                               "y" = c(-0.707, 0.707)),
        "backward_slash" = list("x" = c(0.707, -0.707),
                                "y" = c(-0.707, 0.707)),
        "downward_pentagon" = list("x" = c(0, 0.952, 0.589, -0.589, -0.952, 0),
                          "y" = c(1, 0.309, -0.810, -0.810, 0.309, 1) * -1),
        "downward_hollow_pentagon" = list("x" = c(0, 0.952, 0.589, -0.589, -0.952, 0,
                                         0.952),
                                 "y" = c(1, 0.309, -0.810, -0.810, 0.309, 1,
                                         0.309) * -1),
        "downward_pentagon_cross" = list("x" = c(0, 0, 0, 0.952, 0, 0.589, 0, -0.589,
                                        0, -0.952),
                                "y" = c(0, 1, 0, 0.309, 0, -0.810, 0, -0.810,
                                        0, 0.309) * -1,
                                "poly" = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)),
        "pentagon_hollow_cross" = list("x" = c(0, 0.952, 0.589, -0.589, -0.952, 0,
                                               0.952, 0, 0, 0, 0.952, 0, 0.589, 0, -0.589,
                                               0, -0.952),
                                       "y" = c(1, 0.309, -0.810, -0.810, 0.309, 1,
                                               0.309, 0, 1, 0, 0.309, 0, -0.810, 0, -0.810,
                                               0, 0.309),
                                       "poly" = c(1, 1, 1, 1, 1, 1, 1,
                                                           2, 2, 3, 3, 4, 4, 5, 5, 6, 6)),
        "downward_pentagon_hollow_cross" = list("x" = c(0, 0.952, 0.589, -0.589, -0.952, 0,
                                                        0.952, 0, 0, 0, 0.952, 0, 0.589, 0, -0.589,
                                                        0, -0.952),
                                                "y" = c(1, 0.309, -0.810, -0.810, 0.309, 1,
                                                        0.309, 0, 1, 0, 0.309, 0, -0.810, 0, -0.810,
                                                        0, 0.309) * -1,
                                                "poly" = c(1, 1, 1, 1, 1, 1, 1,
                                                           2, 2, 3, 3, 4, 4, 5, 5, 6, 6)),
        "flip_square_squiggle" = list("x" = c(-0.707, -0.707, 0.707, 0.707) * -1,
                                      "y" = c(0.707, -0.707, 0.707, -0.707))
)


# below copied directly from ggplot2 package 17/03/2023
manual_scale <- function(aesthetic, values = NULL, breaks = waiver(), ..., limits = NULL) {
        # check for missing `values` parameter, in lieu of providing
        # a default to all the different scale_*_manual() functions
        if (is_missing(values)) {
                values <- NULL
        } else {
                force(values)
        }

        if (is.null(limits) && !is.null(names(values))) {
                # Limits as function to access `values` names later on (#4619)
                limits <- function(x) intersect(x, names(values)) %||% character()
        }

        # order values according to breaks
        if (is.vector(values) && is.null(names(values)) && !is.waive(breaks) &&
            !is.null(breaks) && !is.function(breaks)) {
                if (length(breaks) <= length(values)) {
                        names(values) <- breaks
                } else {
                        names(values) <- breaks[1:length(values)]
                }
        }

        pal <- function(n) {
                if (n > length(values)) {
                        cli::cli_abort("Insufficient values in manual scale. {n} needed but only {length(values)} provided.")
                }
                values
        }
        discrete_scale(aesthetic, "manual", pal, breaks = breaks, limits = limits, ...)
}

waiver <- function() structure(list(), class = "waiver")

is.waive <- function(x) inherits(x, "waiver")
