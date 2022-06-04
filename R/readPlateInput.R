#' Read plate input file
#'
#'@param input_file A Victor X3 output file (in format .txt)
#'@returns A tibble
#'@export
#'

readPlateInput <- function(input_file) {

  # Open the 'long table' found in the output file of Victor X3:
  suppressMessages(suppressWarnings(
  plate_rawdata <- readr::read_delim(file = input_file,
                                     delim = "\t",
                                     col_types = "nncctdtdtd",
                                     show_col_types = FALSE)
  ))

  #> finds where the table ends (i.e. where the readr parsing went wrong)
  maxline <- readr::problems(plate_rawdata) %>%
    dplyr::pull(row) %>%
    min() - 1

  #> cleans the input table
  plate_rawdata <- plate_rawdata |>  dplyr::filter(dplyr::row_number() < maxline)

  return(plate_rawdata)

}
