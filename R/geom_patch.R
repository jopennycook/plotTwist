#' Title
#'
#' @param mapping
#' @param data
#' @param stat
#' @param position
#' @param ...
#' @param linejoin
#' @param na.rm
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
geom_patch <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ...,
                       linejoin = "mitre",
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
        layer(
                data = data,
                mapping = mapping,
                stat = stat,
                geom = GeomPatch,
                position = position,
                show.legend = show.legend,
                inherit.aes = inherit.aes,
                params = list2(
                        linejoin = linejoin,
                        na.rm = na.rm,
                        ...
                )
        )
}

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
scale_pattern_manual <- function (..., values, breaks = waiver(), na.value = NA) {
        manual_scale("pattern", values, breaks, ..., na.value = na.value)
}

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
scale_p_colour_manual <- function (..., values, breaks = waiver(), na.value = NA) {
        manual_scale("p_colour", values, breaks, ..., na.value = na.value)
}

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
scale_p_linewidth_manual <- function (..., values, breaks = waiver(), na.value = NA) {
        manual_scale("p_linewidth", values, breaks, ..., na.value = na.value)
}

draw_key_patch <- function(data, params, size) {

        patchGrob(xmin = unit(0.1, "npc"),
                  xmax = unit(0.9, "npc"),
                  ymin = unit(0.1, "npc"),
                  ymax = unit(0.9, "npc"),
                  pattern = data$pattern %||% 1,
                  spacing = unit(data$spacing %||% 1, "mm"),
                  p_colour = data$p_colour %||% "black",
                  p_linewidth = data$p_linewidth %||% 1,
                  fill = data$fill %||% "white",
                  gp = gpar(col = data$colour %||% "black",
                            lwd = data$linewidth %||% 1.42,
                            lty = data$linetype %||% 1))
}

GeomPatch <- ggproto("GeomPatch", Geom,
                     default_aes = aes(colour = "black", fill = "white", linewidth = 1.42, linetype = 1,
                                       alpha = NA, pattern = 1, spacing = 1, p_colour = "black", p_linewidth = 1.42),

                     required_aes = c("xmin", "xmax", "ymin", "ymax"),

                     draw_panel = function(self, data, panel_params, coord, lineend = "butt", linejoin = "mitre") {
                             coords <- coord$transform(data, panel_params)

                             grob_list <- lapply(seq(nrow(coords)), function(i_pattern) {
                                     patchGrob(xmin = unit(coords$xmin[i_pattern], "native"),
                                               xmax = unit(coords$xmax[i_pattern], "native"),
                                               ymin = unit(coords$ymin[i_pattern], "native"),
                                               ymax = unit(coords$ymax[i_pattern], "native"),
                                               pattern = coords$pattern[i_pattern],
                                               spacing = unit(coords$spacing[i_pattern], "mm"),
                                               p_colour = coords$p_colour[i_pattern],
                                               p_linewidth = coords$p_linewidth[i_pattern],
                                               fill = coords$fill[i_pattern],
                                               gp = gpar(col = coords$colour[i_pattern],
                                                         lwd = coords$linewidth[i_pattern],
                                                         lty = coords$linewidth[i_pattern]))
                             })

                             do.call(grobTree, grob_list)
                     },

                     draw_key = draw_key_patch,

                     rename_size = TRUE
)
