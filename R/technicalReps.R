#' calculates all of the technical replicate means
#' @inheritParams plateParser
#' @export
#'

technicalReps <- function(plate_file, plate_map) {

  data <- plateParser(plate_file, plate_map)

  collapsedTechicalReps <- NULL

  for (sample in 1:nrow(data$map_parsed)) {

    wells <- data$map_parsed$sample_wells[[sample]]
    blank <- data$map_parsed$blank_wells[[sample]]
    name <- data$map_parsed$variable[sample]

    collapse <- CollapseTechnicalReps(
      sample_name = name,
      plate = data$rawdata,
      sample_wells = wells,
      blank_wells =  blank
    )
    collapsedTechicalReps <- dplyr::bind_rows(collapsedTechicalReps, collapse)

  }

  data$collapsedTechicalReps <- collapsedTechicalReps

  return(data)
}
