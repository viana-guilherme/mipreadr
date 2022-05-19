#' reads a plate map from a file
#'
#' @param plate_file A VictorX output file
#' @param plate_map A .xlsx spreadsheet mapping samples to wells in a 96 well plate
#' @return a list containing the formatted table map and metadata, as well as the raw data from plate_file
#' @export

plateParser<- function(plate_file, plate_map) {

  # Open the 'long table' found in the output file of Victor X3:
  plate_rawdata <- suppressWarnings(readr::read_delim(file = plate_file, delim = "\t", show_col_types = FALSE))

  #> removing the rows below the table that are not used in the analysis:
  #> finds where the table ends (i.e. where the readr parsing went wrong)
  maxline <- readr::problems(plate_rawdata) %>%
    dplyr::pull(row) %>%
    min() - 1
  #> cleans the input table
  plate_rawdata <- plate_rawdata |>  dplyr::filter(dplyr::row_number() < maxline)

  # Opens the excel map file:
  map <- suppressMessages(readxl::read_excel(plate_map)) %>%
    tibble::column_to_rownames("...1")
  #> turns the long map template into a 2-column tibble
  map_long <- purrr::map_df(.x = 1:nrow(map), ~ {
                      data <- purrr::map[.x,] %>% tidyr::pivot_longer(
                                                          cols = 1:12,
                                                          values_to = "samples",
                                                          names_to = "names") %>%
                                                  dplyr::mutate(
                                                          names = paste0(rownames(map)[.x], names) %>%
                                                          stringr::str_remove_all('\"'))
                      return(data)
                    })

  #>
  unique_samples <- map_long %>% dplyr::select(samples) %>% unique() %>% dplyr::pull()

  sample_to_well <- NULL

  for (sample in unique_samples) {
    wells <- map_long %>%
      dplyr::filter(samples == sample) %>%
      dplyr::select(names) %>%
      dplyr::pull

    subset <- tibble::tibble(sample = sample, wells = list(wells))

    sample_to_well <- dplyr::bind_rows(sample_to_well, subset)
  }


  all_blanks <- unique_samples %>% stats::na.omit() %>% stringr::str_subset("[Bb]lank_", negate = FALSE)
  all_samples <- unique_samples %>% stats::na.omit() %>% stringr::str_subset("[Bb]lank_", negate = TRUE)

  map_parsed <- stringr::str_split(all_samples, "_", simplify = TRUE) %>%
    magrittr::set_colnames(c("sample", "condition")) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(blank = purrr::map_chr(.x = .$condition, ~ {
      blank <- stringr::str_subset(string = all_blanks, pattern = .x)
      return(blank)}),
      variable = stringr::str_c(sample, condition, sep = "_"))

  map_parsed <- dplyr::left_join(x = map_parsed, y = sample_to_well, by = c("variable" = "sample")) %>%
    dplyr::rename(sample_wells = wells) %>%
    dplyr::left_join(y = sample_to_well, by = c("blank" = "sample")) %>%
    dplyr::rename(blank_wells = wells)

  # determine the plate name based on the path to the map files
  platename <- stringr::str_extract(plate_map, pattern = "(?<=/).+(?=.xlsx)") %>%
    stringr::str_replace_all(pattern = "/", replacement = "_")


  message(
    paste0("Finished parsing plate ",
           platename,
           "!\nFound ",
           length(all_samples),
           " unique samples")
  )

  output <- tibble::lst(platename = platename,
                        rawdata = plate_rawdata,
                        map_parsed = map_parsed
  )

  return(output)
}
