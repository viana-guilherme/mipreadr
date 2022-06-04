#' Calculate technical replicate means for a single sample
#'
#' @param sample_name The name for the sample to be collapsed
#' @param plate The plate rawdata from which to calculate means
#' @param sample_wells A character vector pointing the coordinates (e.g. "A01", "B02"...) for the sample within the plate
#' @param blank_wells A character vector pointing the coordinates for the wells of the 'blank'
#' @param normalize (logical) Should the fluorescence values be normalized by the absorbance?
#' @return A small tibble with the calculated mean for the technical replicates
#'

collapseTechnicalReps <- function(sample_name, plate, sample_wells, blank_wells, normalize = TRUE) {

  # determining columns of interest for the function (absorbance, and fluorescence measurements)
  colnames_to_use <- colnames(plate) |>
    stringr::str_replace_all(pattern = " ", replacement = "") |>
    stringr::str_extract_all(pattern = ".+(?=\\(Counts\\)|@)", simplify = TRUE) |>
    stringr::str_remove_all(pattern = '""') |>
    dplyr::as_tibble() |>
    dplyr::bind_cols(column_names = colnames(plate)) |>
    dplyr::filter(value != "")

  # for each of the columns of interest, calculate the mean and subtract the blank measurement

  for (i in 1:nrow(colnames_to_use))  {

    varName <- colnames_to_use[i,1] |> dplyr::pull()
    colName <- colnames_to_use[i,2] |> dplyr::pull()

    # sample measurements
    sample <- plate |>
        dplyr::filter(Well %in% sample_wells)  |>
        dplyr::group_by(Repeat) |>
        dplyr::summarise(Mean = mean(.data[[colName]]))

    # blank measurements
    blank <- plate |>
        dplyr::filter(Well %in% blank_wells)  |>
        dplyr::group_by(Repeat) |>
        dplyr::summarise(Mean = mean(.data[[colName]]))

    assign(x = varName, value = {
      sample |>
      dplyr::mutate(Variable = sample_name,
                    Mean = Mean - blank$Mean)  |>
      dplyr::relocate(Variable, .before = Mean)|>
      dplyr::rename_with(.fn = ~ eval(varName), .cols = Mean)
    })
  }

  # unite the measurements into a single table
  sample_mean <- NULL

  for (idx in 1:nrow(colnames_to_use)) {
    if (is.null(sample_mean)) {
      sample_mean <- get(colnames_to_use$value[idx])
    } else {
      sample_mean <- dplyr::left_join(sample_mean, get(colnames_to_use$value[idx]), by = c("Repeat", "Variable"))
    }
  }

  # normalize fluorescence by absorbance values

  if (normalize) {

    abs <- stringr::str_subset(colnames(sample_mean), pattern = "Absorbance")
    fluo <- stringr::str_subset(colnames(sample_mean), pattern = "Absorbance|Repeat|Variable", negate = TRUE)

    for (f in 1:length(fluo)) {
      column_name <- paste0(fluo[f],"/",abs)

      sample_mean <- sample_mean |>
        dplyr::mutate(normalized = .data[[fluo[f]]]/.data[[abs]]) |>
        dplyr::rename_with(.fn = ~ eval(column_name), .cols = normalized)

    }
  }

  return(sample_mean)
}




