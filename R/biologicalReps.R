#' calculates all of the biological replicate means
#' @param x A list generated with technicalReps
#' @export
#'
biologicalReps <- function(x) {

#   for( ) {
#
  suppressMessages(x$collapsedTechnicalReps  |>
    dplyr::group_by(Sample, Condition, Repeat) |>
    dplyr::summarise(BiologicalRepMean = mean(Mean)) -> biological_mean)

  return(biological_mean)


}
