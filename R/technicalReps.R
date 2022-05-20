#' calculates all of the technical replicate means
#' @inheritParams plateParser
#' @export
#'

technicalReps <- function(plate_file, plate_layout) {

  data <- plateParser(plate_file, plate_layout)

  collapsedTechicalReps <- NULL

  for (sample in 1:nrow(data$plate_layout_parsed)) {

    wells <- data$plate_layout_parsed$sample_wells[[sample]]
    blank <- data$plate_layout_parsed$blank_wells[[sample]]
    name <- data$plate_layout_parsed$variable[[sample]]
    replicate <- data$plate_layout_parsed$Replicate[[sample]]

    collapse <- collapseTechnicalReps(
      sample_name = name,
      plate = data$rawdata,
      sample_wells = wells,
      blank_wells =  blank
    ) |>
      dplyr::mutate(Replicate = replicate)

    collapsedTechicalReps <- dplyr::bind_rows(collapsedTechicalReps, collapse)

  }

  data$collapsedTechicalReps <- collapsedTechicalReps

  return(data)
}
