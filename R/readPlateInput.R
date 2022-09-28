#' Read plate input file
#'
#'@param input_file A microplate reader output file (in formats .txt or .csv)
#'@param mode Which parsing function to use. Currently supports "x3" for Victor X3 conventional output file, or "nivo" for Victor Nivo files
#'@param delimiter Field delimiter used in the file
#'@param point_measure for mode = "nivo", tells whether the output file is a point measurement, or if there are several points into the same file
#'@returns A tibble
#'@export
#'

readPlateInput <- function(input_file, mode = "x3", delimiter = "\t", point_measure = FALSE) {

  if (mode == 'x3') {
    # Open the 'long table' found in the output file of Victor X3:
    suppressMessages(suppressWarnings(
    plate_rawdata <- readr::read_delim(file = input_file,
                                       delim = delimiter,
                                       col_types = "nncctdtdtd",
                                       show_col_types = FALSE)
    ))

    #> finds where the table ends (i.e. where the readr parsing went wrong)
    maxline <- readr::problems(plate_rawdata) %>%
      dplyr::pull(row) %>%
      min() - 1

    #> cleans the input table
    plate_rawdata <- plate_rawdata |>  dplyr::filter(dplyr::row_number() < maxline)

  } else if (mode == "nivo") {

    plate_rawdata <-  nivoParser(input_file)

  } else {
    stop('Please enter a valid parsing mode (current accepted values are "x3" or "nivo")', call. = FALSE)
  }

  return(plate_rawdata)

}
