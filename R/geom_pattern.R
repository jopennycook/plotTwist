#' Title
#'
#' @param mapping
#' @param data
#' @param stat
#' @param position
#' @param ...
#' @param just
#' @param width
#' @param na.rm
#' @param orientation
#' @param show.legend
#' @param inherit.aes
#'
#' @return
#' @import ggplot2
#' @import grid
#' @import rlang
#' @import vctrs
#' @export
#'
#' @examples
geom_pattern <- function(mapping = NULL, data = NULL,
                     stat = "count", position = "stack",
                     ...,
                     just = 0.5,
                     width = NULL,
                     na.rm = FALSE,
                     orientation = NA,
                     show.legend = NA,
                     inherit.aes = TRUE) {
        layer(
                data = data,
                mapping = mapping,
                stat = stat,
                geom = GeomPattern,
                position = position,
                show.legend = show.legend,
                inherit.aes = inherit.aes,
                params = list2(
                        just = just,
                        width = width,
                        na.rm = na.rm,
                        orientation = orientation,
                        ...
                )
        )
}

GeomPattern <- ggproto("GeomPattern", GeomPatch,
                   required_aes = c("x", "y"),

                   non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),

                   setup_params = function(data, params) {
                           params$flipped_aes <- has_flipped_aes(data, params)
                           params
                   },

                   extra_params = c("just", "na.rm", "orientation"),

                   setup_data = function(data, params) {
                           data$flipped_aes <- params$flipped_aes
                           data <- flip_data(data, params$flipped_aes)
                           data$width <- data$width %||%
                                   params$width %||% (resolution(data$x, FALSE) * 0.9)
                           data$just <- params$just %||% 0.5
                           data <- transform(data,
                                             ymin = pmin(y, 0), ymax = pmax(y, 0),
                                             xmin = x - width * just, xmax = x + width * (1 - just),
                                             width = NULL, just = NULL
                           )
                           flip_data(data, params$flipped_aes)
                   },

                   draw_panel = function(self, data, panel_params, coord, lineend = "butt",
                                         linejoin = "mitre", width = NULL, flipped_aes = FALSE) {
                           # Hack to ensure that width is detected as a parameter
                           ggproto_parent(GeomPatch, self)$draw_panel(
                                   data,
                                   panel_params,
                                   coord,
                                   lineend = lineend,
                                   linejoin = linejoin
                           )
                   },
                   rename_size = TRUE
)
