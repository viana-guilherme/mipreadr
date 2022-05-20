#' calculates all of the technical replicate means
#' @inheritParams plateParser
#' @export
#'

technicalReps <- function(plate_file, plate_layout) {

  data <- plateParser(plate_file, plate_layout)

  collapsedTechnicalReps <- NULL

  for (smp in 1:nrow(data$plate_layout_parsed)) {

    wells <- data$plate_layout_parsed$sample_wells[[smp]]
    blank <- data$plate_layout_parsed$blank_wells[[smp]]
    name <- data$plate_layout_parsed$variable[[smp]]
    sample <- data$plate_layout_parsed$Sample[[smp]]
    replicate <- data$plate_layout_parsed$Replicate[[smp]]
    condition <- data$plate_layout_parsed$Condition[[smp]]

    collapse <- collapseTechnicalReps(
      sample_name = name,
      plate = data$rawdata,
      sample_wells = wells,
      blank_wells =  blank
    ) |>
      dplyr::mutate(Sample = sample,
                    Condition = condition,
                    Replicate = replicate,
                    ) |>
      dplyr::relocate(Variable, Sample, Condition, Replicate, Repeat, Mean)

    collapsedTechnicalReps <- dplyr::bind_rows(collapsedTechnicalReps, collapse)

  }

  data$collapsedTechnicalReps <- collapsedTechnicalReps

  return(data)
}
