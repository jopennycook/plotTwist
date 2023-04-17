#' Title
#'
#' @param model
#' @param parameters
#'
#' @return
#' @export
#'
#' @examples
predict_eb <- function(model, parameters) {
        new_data <- do.call(expand.grid, parameters)

        predicted_data <- predict(model,
                                  new_data,
                                  se.fit = TRUE,
                                  type = "link",
                                  re.form = NA)

        new_data$linkmean <- predicted_data$fit
        new_data$linkse <- predicted_data$se.fit
        new_data$linklower <- predicted_data$fit - 1.96 * predicted_data$se.fit
        new_data$linkupper <- predicted_data$fit + 1.96 * predicted_data$se.fit
        new_data$mean <- exp(new_data$linkmean)
        new_data$lower <- exp(new_data$linklower)
        new_data$upper <- exp(new_data$linkupper)

        new_data
}
