#' Title
#'
#' @param libname
#' @param pkgname
#'
#' @return
#' @import showtext
#' @import sysfonts
#'
#' @examples
.onLoad <- function(libname, pkgname) {
#        font_add_google("Nunito", family = "Nunito200", regular.wt = 200)
#        font_add_google("Nunito", family = "Nunito300", regular.wt = 300)
#        font_add_google("Nunito", family = "Nunito400", regular.wt = 400)
        font_add_google("Nunito", family = "Nunito500", regular.wt = 500)
#        font_add_google("Nunito", family = "Nunito600", regular.wt = 600)
        font_add_google("Nunito", family = "Nunito700", regular.wt = 700)
#        font_add_google("Nunito", family = "Nunito800", regular.wt = 800)

        showtext_auto(TRUE)
}

.onUnload <- function(libname, pkgname) {
        showtext_auto(FALSE)
}
