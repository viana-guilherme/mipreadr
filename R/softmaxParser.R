#' Parse SoftMax's plate reader software long data format [EXPERIMENTAL]
#'
#'@param input_file A  SoftMax reader output file (when exproting, select long column format)
#'@returns A tibble

# setting up auxiliary functions
timeToRepeats <- function(string) {
  nrepeats <- unique(stringr::str_remove_all(string, pattern = ":\\d{2}$"))
  purrr::map_dbl(string, ~ which(stringr::str_starts(.x, pattern = nrepeats)))
}

convertHours <- function(string) {

  timeshift <- hms::as_hms("24:00:00")

  # identify if it has 1. or 2. at the beginning
  if (!stringr::str_starts(string, "\\d\\.")) {

    newTime <- hms::as_hms(string)

  }  else {

    offset <- stringr::str_extract(string, pattern = "\\d(?=\\.)") |> as.numeric() # identifies the faulty number
    Time <- stringr::str_remove_all(string, pattern = "\\d\\.") |>  hms::as_hms() # removes the faulty number

    newTime <- hms::as_hms(Time + timeshift * offset)

    return(newTime)
  }
}


formatWellNames <- function(string) {
  stringr::str_split_fixed(string, pattern = "", n = 2) |>
    as.character() |>
    stringr::str_flatten(collapse = "0")
} # add format 01, 02... Obs: currently only works reliably for 48-well plates

## importing growth data
softmaxParser <- function(input_file) {

  readr::read_delim(
  file = input_file,
  skip = 2, delim = "\t", locale = readr::locale(encoding = "utf-16le")) |>
  dplyr::select(-`Temperature(¡C)`, -`...51`) |>
  tidyr::pivot_longer(cols = A1:F8, names_to = "Well", values_to = "Absorbance (A)") |>
  stats::na.omit() |>
  dplyr::mutate(Plate = 1,
                Repeat = timeToRepeats(Time),
                Time = purrr::map_vec(Time, convertHours),
                Well = purrr::map_vec(Well, formatWellNames)) |>
  dplyr::relocate(Plate, Repeat, Well, Time, `Absorbance (A)`)
}
