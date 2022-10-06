#' reads a plate plate layout from a file
#'
#' @param file A VictorX output file
#' @param layout A .xlsx spreadsheet mapping samples to wells in a 96 well plate
#' @inheritParams readPlateInput
#' @return a list containing the formatted table map and metadata, as well as the raw data from plate_file
#' @export

plateParser <- function(file, layout, mode = "x3", delimiter = "\t", point_measure = FALSE) {

  # reading the inputs
  plate_rawdata <- readPlateInput(file, mode, delimiter, point_measure)
  plate_layout_long <- elongateLayout(layout)

  # find the unique sample names in the plate layout input
  unique_samples <- plate_layout_long |>
                      dplyr::select(Samples) |>
                      unique() |>
                      dplyr::pull()

  # mapping the unique samples to their respective wells, saves in a tibble
  mapped_samples <- NULL
  for (sample in unique_samples) {

    wells <- plate_layout_long  |>
      dplyr::filter(Samples == sample)  |>
      dplyr::select(Wells) |>
      dplyr::pull()

    sample_to_well <- tibble::tibble(variable = sample, wells = list(wells))

    mapped_samples <- dplyr::bind_rows(mapped_samples, sample_to_well)
  }

  # separating blanks (#) from the remaining samples
  all_blanks <- unique_samples |> stringr::str_subset("#", negate = FALSE)
  all_samples <- unique_samples |>  stringr::str_subset("#", negate = TRUE)


  # parsing the names given in the plate layout to be used as metadata

  suppressMessages(
    plate_layout_parsed <- stringr::str_split(all_samples, "_", simplify = TRUE) |>
    tibble::as_tibble(.name_repair = "unique") |>
    dplyr::mutate(variable = all_samples) |>
    dplyr::relocate(variable)
    )

  # checks if Replicate numbers were manually given by the user
  if (ncol(plate_layout_parsed) == 4) {

    Replicate_number <- purrr::map_dbl(
      .x = as.numeric(plate_layout_parsed$...3), ~ ifelse(is.na(.x), 1, .x))

    } else {

    Replicate_number <- rep(1, times = nrow(plate_layout_parsed))

    }

  # assembles the plate metadata tibble
  plate_metadata <- tibble::tibble(variable = plate_layout_parsed[[1]],
                                   Sample = plate_layout_parsed[[2]],
                                   Condition = plate_layout_parsed[[3]],
                                   Replicate = Replicate_number)

  # attaching the corresponding blanks to the samples
  plate_metadata <- dplyr::mutate(plate_metadata,
                                       blank = purrr::map_chr(.x = paste0("#", plate_metadata$Condition), ~ {
                                                      Blank <- stringr::str_subset(string = all_blanks,
                                                                                   pattern = .x)
                                                      return(Blank)}))

  # attaching the wells to each sample, as stored in mapped_samples
  suppressMessages(plate_metadata <- dplyr::left_join(x = plate_metadata, y = mapped_samples) |>
    dplyr::rename(sample_wells = wells) |>
    dplyr::left_join(y = mapped_samples, by = c("blank" = "variable")) |>
    dplyr::rename(blank_wells = wells))

  # determine the plate name based on the path to the map files
  platename <- stringr::str_extract(layout, pattern = "\\w.+(?=.xlsx|.xls)") |>
    stringr::str_replace_all(pattern = "/", replacement = "_")

  # finishing the function
  message(
    paste0("Finished parsing plate ",
           platename,
           "!\nFound ",
           length(all_samples),
           " unique samples:\n",
           stringr::str_flatten(all_samples, collapse = ",\n"))
  )

  output <- tibble::lst(platename = platename,
                        rawdata = plate_rawdata,
                        plate_layout_parsed = plate_metadata)

  return(output)
}
