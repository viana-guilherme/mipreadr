#' Parse Victor Nivo long output file format
#' @inheritParams readPlateInput

nivoParser <- function(input_file, delimiter = "\t", point_measure = FALSE) {

  input_nivo <- readr::read_lines(input_file, skip_empty_rows = TRUE)

  # THE FIRST PART OF THE FUNCTION IS PARSING THE FILE AND IDENTIFYING ITS KEY PARTS

  # file structure
  # 1. "metadata"
  # 2. measurements         < can be repeated for protocols with more than one measurement type
  # 3. parameters for (2.)  < can be repeated for protocols with more than one measurement type
  # 4. descriptions for each step in the protocol
  # 5. plate map            < if exported by the user

  # retrieving the field delimiter indexes
  operationCatcher <- stringr::str_which(input_nivo, pattern = "\\d\\sOPERATION")
  parameterCatcher <- stringr::str_which(input_nivo, pattern = "^PARAMETERS")
  commentCatcher <- stringr::str_which(input_nivo, pattern = "PROTOCOL COMMENT")
  plateCatcher <- stringr::str_which(input_nivo, pattern = "PLATE MAP")
  calculationCatcher <- stringr::str_which(input_nivo, pattern = "CALCULATED")

  # fMatch and fDelim are going to give us the indexes to iterate over
  fMatch <- c(operationCatcher, parameterCatcher, commentCatcher, plateCatcher, calculationCatcher) |>
      sort() |>
    unique()

  fDelim <- fMatch - 1

  # associating the iterators into a single tibble for tidiness
  tblWalk <- tibble::tibble(match = c(1, fMatch), delim = c(fDelim, length(input_nivo)))


  # now on to retrieve the actual data from the results
  input_lst <- purrr::map2( .x = tblWalk$match, .y = tblWalk$delim, ~{
    input_nivo[.x:.y] |>
      stringr::str_split(pattern = delimiter, simplify = TRUE) |>
      tibble::as_tibble(.name_repair = ~ make.names(., unique = TRUE)) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ dplyr::na_if(., y = ""))) #fixed as a more generic solution
      }
     )

  # input results <- this relies entirely on the fact that there is a "well" string on the first column of the parsed output file
  result_idxs <- input_lst |>
    purrr::map(1) |>
    purrr::map_lgl(function(x) "Well" %in% x)

  pointOpt <- point_measure

  if (sum(result_idxs) == 1) { # this means whether only Abs was measured...
    reAssembledOutput <- purrr::keep(input_lst, result_idxs) |>
      purrr::flatten_dfr() |>
      tidyNivo(point_measure = pointOpt)
  } else { # this is important should we have more than one measurement
    result <- purrr::keep(input_lst, result_idxs)
    result_formatted <- purrr::map(result, ~ tidyNivo(.x, point_measure = pointOpt))

    reAssembledOutput <- purrr::reduce(.x = result_formatted, dplyr::left_join, by = c("Cycle", "Plate", "Barcode", "X")) |>
      dplyr::rename(`Time(s)` = `Time(s).x`) |> # 'Canonizes' the first time column as the plate time
      dplyr::select(!matches("Time\\(s\\).+$")) # excludes Time(s) followed by anything
  }


  # ## TODO: Somehow include the entire file metadata into the output of this function
  # metadata_idxs <- input_lst |>
  #   purrr::map(1) |>
  #   purrr::map_lgl(function(x) !("Well" %in% x ))
  #
  # metadata <- purrr::keep(input_lst, metadata_idxs)


  ### THE SECOND PART IS MAKING THE RESULTS TIDIER




    plate_rawdata <- reAssembledOutput |>
      dplyr::rename(Repeat = Cycle, Well = X) |>
      dplyr::rename_with(
        ~ stringr::str_remove_all(.x, "\\s|-.*")
      ) |>
      dplyr::rename_with(
        ~ dplyr::if_else(stringr::str_starts(.x, "ABS"),
                         "Absorbance (A)",
                         .x)
        ) |>
        dplyr::rename_with(
          ~ dplyr::if_else(stringr::str_starts(.x, "FI"),
                         "Fluorescence (Counts)",
                         .x)
      ) |>
      dplyr::mutate(
        dplyr::across(!Well, as.numeric),
        Well = stringr::str_extract(Well, pattern = "\\d+") |>
          purrr::map_chr( ~ dplyr::if_else(stringr::str_length(.x) == 1, paste0("0", .x), .x)) |>
          purrr::map2_chr(.y = stringr::str_extract(Well, "\\w"), ~ stringr::str_c(.y, .x)),
        Time = hms::as_hms(`Time(s)`)
      ) |>
      dplyr::select(-`Time(s)`) |>
      dplyr::relocate(Plate, Repeat, Well, Barcode, Time)


  return(plate_rawdata)
}

tidyNivo <- function(results_list, point_measure = point_measure) {

  if (point_measure) {

    # this is the measurement name for creating the column that will house the data
    measurement <- results_list |>
      dplyr::filter(stringr::str_detect(X, pattern = "OPERATION")) |>
      dplyr::select(X.1) |>
      dplyr::pull()

    # getting the values
    plate_rawdata <- results_list |>
      dplyr::filter(!is.na(X.1), !stringr::str_length(X) > 3) |>
      dplyr::rename_with(~ tidyselect::all_of(c("Well", measurement))) |>
      dplyr::rename_with(
        ~ dplyr::if_else(stringr::str_starts(.x, "ABS"), "Absorbance (A)", .x)) |>
      dplyr::mutate(
        dplyr::across(!Well, as.numeric),
        Well = stringr::str_extract(Well, pattern = "\\d+") |>
          purrr::map_chr( ~ dplyr::if_else(stringr::str_length(.x) == 1, paste0("0", .x), .x)) |>
          purrr::map2_chr(.y = stringr::str_extract(Well, "\\w"), ~ stringr::str_c(.y, .x))
      )

  } else {

    # categories
    # 1st: only two first fields are filled (Metadata)
    metadata <- results_list |>
      dplyr::filter(!is.na(X) & !is.na(X.1) & is.na(X.2)) |>
      janitor::remove_empty("cols")

    ## the measurement name will be the second element of the first row
    measure_name <- metadata |>
      dplyr::filter(stringr::str_detect(X, pattern = "OPERATION")) |>
      dplyr::select(X.1) |>
      dplyr::pull()

    # 2nd: first field is blank (Plate, Barcode, Cycle)
    operation_vars <- results_list |>
      dplyr::filter(is.na(X)) |>
      janitor::remove_empty("cols") |>
      tidyr::pivot_longer(cols = !X.1, names_to = "index") |>
      tidyr::pivot_wider(names_from = X.1, values_from = value)

    # 3rd: all fields are filled (Well + time(s))
    time_vars <- results_list |>
      stats::na.omit() |>
      janitor::remove_empty("cols") |>
      dplyr::select(!X) |>
      tidyr::pivot_longer(cols = !X.1, names_to = "index") |>
      tidyr::pivot_wider(names_from = X.1, values_from = value)

    # 4th: second field is blank (Well data)
    well_data <- results_list |>
      dplyr::filter(is.na(X.1)) |>
      janitor::remove_empty("cols") |>
      tidyr::pivot_longer(cols = !X, names_to = "index", values_to = measure_name)


    reAssembledOutput <- purrr::reduce(.x = list(operation_vars, time_vars, well_data),
                                       .f = dplyr::full_join) |>
      dplyr::select(-index)
  }

  return(reAssembledOutput)
}
