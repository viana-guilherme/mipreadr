#' Calculate technical replicate means for a single sample
#'
#' @param sample_name The name for the sample to be collapsed
#' @param plate The plate rawdata from which to calculate means
#' @param sample_wells A character vector pointing the coordinates (e.g. "A01", "B02"...) for the sample within the plate
#' @param blank_wells A character vector pointing the coordinates for the wells of the 'blank'
#'
#' @return A small tibble with the calculated mean for the technical replicates
#' @export

CollapseTechnicalReps <- function(sample_name, plate, sample_wells, blank_wells) {

  plate |>
    dplyr::filter(Well %in% sample_wells)  |>
    dplyr::group_by(Repeat) |>
    dplyr::summarise(Mean = mean(`Absorbance @ 600 (A)`)) -> sample_mean

  plate  |>
    dplyr::filter(Well %in% blank_wells)  |>
    dplyr::group_by(Repeat)  |>
    dplyr::summarise(Mean = mean(`Absorbance @ 600 (A)`)) -> blank_mean


  sample_mean <- sample_mean |>
    dplyr::mutate(Mean = Mean - blank_mean$Mean,
                  Sample = sample_name)  |>
    dplyr::relocate(Sample, .before = Mean)


  return(sample_mean)
}




