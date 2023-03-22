#' Title
#'
#' @param xmin
#' @param xmax
#' @param ymin
#' @param ymax
#' @param pattern
#' @param spacing
#' @param p_linewidth
#' @param p_colour
#' @param name
#' @param gp
#' @param vp
#'
#' @return
#' @import ggplot2
#' @import grid
#' @import rlang
#' @import vctrs
#' @export
#'
#' @examples
patchGrob <- function(xmin = unit(0.2, "npc"),
                      xmax = unit(0.8, "npc"),
                      ymin = unit(0.2, "npc"),
                      ymax = unit(0.8, "npc"),
                      pattern = "e_hori",
                      spacing = unit(2, "mm"),
                      p_linewidth = 0.5,
                      p_colour = "black",
                      fill = "black",
                      name = NULL,
                      gp = NULL,
                      vp = NULL) {

        grob(xmin = xmin,
             xmax = xmax,
             ymin = ymin,
             ymax = ymax,
             pattern = pattern,
             spacing = spacing,
             p_linewidth = p_linewidth,
             p_colour = p_colour,
             fill = fill,
             name = name,
             gp = gp,
             vp = vp,
             cl = "patch")
}

#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
drawDetails.patch <- function(x, ...) {
        pattern_recipe <- pattern_recipes[[x$pattern]]

        mm_xmin <- convertX(x$xmin, "mm", valueOnly = TRUE)
        mm_xmax <- convertX(x$xmax, "mm", valueOnly = TRUE)
        mm_ymin <- convertY(x$ymin, "mm", valueOnly = TRUE)
        mm_ymax <- convertY(x$ymax, "mm", valueOnly = TRUE)
        mm_spacing <- convertY(x$spacing, "mm", valueOnly = TRUE)

        # convert from xmin etc. to the relevant polygon coordinates
        polymap <- rect_to_poly(mm_xmin,
                                mm_xmax,
                                mm_ymin,
                                mm_ymax)

        if (!pattern_recipe$fill) {
                x$fill <- "#00000000"
        }
        # create the base rectangle graphical object according to parameters
        grid.polygon(x = polymap$x,
                     y = polymap$y,
                     default.units = "mm",
                     gp = gpar(fill = x$fill))

        # otherwise calculate the appropriate stripe coordinates
        stripemap <- stripe_points(width = mm_xmax - mm_xmin,
                                   height = mm_ymax - mm_ymin,
                                   spacing = mm_spacing * pattern_recipe$space_mult,
                                   alt = pattern_recipe$alt,
                                   stripe = pattern_recipe$stripe,
                                   orth = pattern_recipe$orth,
                                   hatch = pattern_recipe$hatch)

        # layer stripes on top of rectangle
        grid.polyline(x = c(t(stripemap[, c(1, 3)])) + mm_xmin,
                      y = c(t(stripemap[, c(2, 4)])) + mm_ymin,
                      id = rep(1:nrow(stripemap), each = 2),
                      default.units = "mm",
                      gp = gpar(col = x$p_colour,
                                lwd = x$p_linewidth))

        grid.polygon(x = polymap$x,
                     y = polymap$y,
                     default.units = "mm",
                     gp = gpar(fill = "#00000000"))
}

data_frame0 <- function(...) data_frame(..., .name_repair = "minimal")

rect_to_poly <- function(xmin, xmax, ymin, ymax) {
        data_frame0(
                y = c(ymax, ymax, ymin, ymin, ymax, ymax),
                x = c(xmin, xmax, xmax, xmin, xmin, xmax)
        )
}

diag_stripe_points <- function(width, height, spacing, flip = FALSE) {
        outline_spacing <- (2 * (spacing ^ 2)) ^ 0.5

        if (outline_spacing < width) {
                width_points <- seq(outline_spacing, width, outline_spacing)
                w_partial <- outline_spacing - (width - tail(width_points, 1))
        } else {
                width_points <- numeric()
                w_partial <- outline_spacing - width
        }

        if(w_partial < height) {
                height2_points <- seq(w_partial,
                                      height, outline_spacing)
        } else {
                height2_points <- numeric()
        }

        if (outline_spacing < height) {
                height_points <- seq(outline_spacing, height, outline_spacing)
                h_partial <- outline_spacing - (height - tail(height_points, 1))
        } else {
                height_points <- numeric()
                h_partial <- outline_spacing - height
        }

        if(h_partial < width) {
                width2_points <- seq(h_partial,
                                     width, outline_spacing)
        } else {
                width2_points <- numeric()
        }

        from_x <- c(rep(0, length(height_points)), width2_points)
        from_y <- c(height_points, rep(height, length(width2_points)))
        to_x <- c(width_points, rep(width, length(height2_points)))
        to_y <- c(rep(0, length(width_points)), height2_points)

        if(flip == TRUE) {
                from_y <- height - from_y
                to_y <- (to_y - height) * -1
        }

        data.frame("from_x" = from_x,
                   "from_y" = from_y,
                   "to_x" = to_x,
                   "to_y" = to_y)
}

orth_stripe_points <- function(width, height, spacing, hori = FALSE) {

        if (hori == TRUE) {
                from_x <- seq(0, width, spacing)
                to_x <- from_x
                from_y <- rep(height, length(from_x))
                to_y <- rep(0, length(from_x))
        } else {
                from_y <- seq(0, height, spacing)
                to_y <- from_y
                from_x <- rep(width, length(from_y))
                to_x <- rep(0, length(from_y))
        }

        data.frame("from_x" = from_x,
                   "from_y" = from_y,
                   "to_x" = to_x,
                   "to_y" = to_y)
}

stripe_points <- function(width, height, spacing, alt = F, stripe = T, orth = T, hatch = F) {

        if (stripe) {
                if (orth) {
                        if (hatch) {
                                output <- rbind(orth_stripe_points(width, height, spacing, T),
                                                orth_stripe_points(width, height, spacing, F))
                        } else {
                                output <- orth_stripe_points(width, height, spacing, alt)
                        }
                } else {
                        if (hatch) {
                                output <- rbind(diag_stripe_points(width, height, spacing, T),
                                                diag_stripe_points(width, height, spacing, F))
                        } else {
                                output <- diag_stripe_points(width, height, spacing, alt)
                        }
                }
        } else {
                output <- orth_stripe_points(0, 0, 0)
        }

        output
}

pattern_recipes <- list(
        "fill" = list("fill" = T,
                      "stripe" = F),
        "f_hori" = list("fill" = T,
                        "stripe" = T,
                        "space_mult" = 1,
                        "alt" = F,
                        "orth" = T,
                        "hatch" = F),
        "f_vert" = list("fill" = T,
                        "stripe" = T,
                        "space_mult" = 1,
                        "alt" = T,
                        "orth" = T,
                        "hatch" = F),
        "f_fdiag" = list("fill" = T,
                         "stripe" = T,
                         "space_mult" = 0.9,
                         "alt" = T,
                         "orth" = F,
                         "hatch" = F),
        "f_bdiag" = list("fill" = T,
                         "stripe" = T,
                         "space_mult" = 0.9,
                         "alt" = F,
                         "orth" = F,
                         "hatch" = F),
        "f_hatch" = list("fill" = T,
                         "stripe" = T,
                         "space_mult" = 1,
                         "alt" = F,
                         "orth" = T,
                         "hatch" = T),
        "f_diaghatch" = list("fill" = T,
                             "stripe" = T,
                             "space_mult" = 0.9,
                             "alt" = F,
                             "orth" = F,
                             "hatch" = T),
        "empty" = list("fill" = F,
                       "stripe" = F),
        "e_hori" = list("fill" = F,
                        "stripe" = T,
                        "space_mult" = 1,
                        "alt" = F,
                        "orth" = T,
                        "hatch" = F),
        "e_vert" = list("fill" = F,
                        "stripe" = T,
                        "space_mult" = 1,
                        "alt" = T,
                        "orth" = T,
                        "hatch" = F),
        "e_fdiag" = list("fill" = F,
                         "stripe" = T,
                         "space_mult" = 0.9,
                         "alt" = T,
                         "orth" = F,
                         "hatch" = F),
        "e_bdiag" = list("fill" = F,
                         "stripe" = T,
                         "space_mult" = 0.9,
                         "alt" = F,
                         "orth" = F,
                         "hatch" = F),
        "e_hatch" = list("fill" = F,
                         "stripe" = T,
                         "space_mult" = 1,
                         "alt" = F,
                         "orth" = T,
                         "hatch" = T),
        "e_diaghatch" = list("fill" = F,
                             "stripe" = T,
                             "space_mult" = 0.9,
                             "alt" = F,
                             "orth" = F,
                             "hatch" = T),
        "f_hori_2" = list("fill" = T,
                          "stripe" = T,
                          "space_mult" = 2,
                          "alt" = F,
                          "orth" = T,
                          "hatch" = F),
        "f_vert_2" = list("fill" = T,
                          "stripe" = T,
                          "space_mult" = 2,
                          "alt" = T,
                          "orth" = T,
                          "hatch" = F),
        "f_fdiag_2" = list("fill" = T,
                           "stripe" = T,
                           "space_mult" = 1.8,
                           "alt" = T,
                           "orth" = F,
                           "hatch" = F),
        "f_bdiag_2" = list("fill" = T,
                           "stripe" = T,
                           "space_mult" = 1.8,
                           "alt" = F,
                           "orth" = F,
                           "hatch" = F),
        "f_hatch_2" = list("fill" = T,
                           "stripe" = T,
                           "space_mult" = 2,
                           "alt" = F,
                           "orth" = T,
                           "hatch" = T),
        "f_diaghatch_2" = list("fill" = T,
                               "stripe" = T,
                               "space_mult" = 1.8,
                               "alt" = F,
                               "orth" = F,
                               "hatch" = T),
        "e_hori_2" = list("fill" = F,
                          "stripe" = T,
                          "space_mult" = 2,
                          "alt" = F,
                          "orth" = T,
                          "hatch" = F),
        "e_vert_2" = list("fill" = F,
                          "stripe" = T,
                          "space_mult" = 2,
                          "alt" = T,
                          "orth" = T,
                          "hatch" = F),
        "e_fdiag_2" = list("fill" = F,
                           "stripe" = T,
                           "space_mult" = 1.8,
                           "alt" = T,
                           "orth" = F,
                           "hatch" = F),
        "e_bdiag_2" = list("fill" = F,
                           "stripe" = T,
                           "space_mult" = 1.8,
                           "alt" = F,
                           "orth" = F,
                           "hatch" = F),
        "e_hatch_2" = list("fill" = F,
                           "stripe" = T,
                           "space_mult" = 2,
                           "alt" = F,
                           "orth" = T,
                           "hatch" = T),
        "e_diaghatch_2" = list("fill" = F,
                               "stripe" = T,
                               "space_mult" = 1.8,
                               "alt" = F,
                               "orth" = F,
                               "hatch" = T))
