#' Title
#'
#' @return
#' @import ggplot2
#' @import ggtext
#' @export
#'
#' @examples
simple_theme <- function() {
        theme(axis.title.x = element_markdown(family = "sans", size = 12),
              axis.title.y = element_markdown(family = "sans", size = 12),
              axis.text.x = element_markdown(family = "sans", size = 9, colour = "black"),
              axis.text.y = element_markdown(family = "sans", size = 9, colour = "black"),
              axis.line.x = element_blank(),
              axis.line.y = element_blank(),
              axis.ticks = element_line(linewidth = 0.5, colour = "black"),
              strip.text.x = element_markdown(family = "sans", size = 9, colour = "black"),
              strip.text.y = element_markdown(family = "sans", size = 9, colour = "black"),
              strip.background = element_blank(),
              panel.grid = element_blank(), panel.border = element_blank(),
              panel.background = element_rect(fill = "white", colour = "black", linewidth = 1),
              legend.background = element_blank(), legend.key = element_blank(),
              legend.title = element_markdown(family = "sans", size = 12),
              legend.title.align = 0.5,
              legend.text = element_markdown(family = "sans", size = 9, colour = "black"))
}
