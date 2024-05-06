#' Parse Biotek's plate reader software long data format [EXPERIMENTAL]
#'
#'@param input_file A  Biotek reader output file (when exproting, select long column format)
#'@returns A tibble

biotekParser <- function(input_file) {

  data <- readr::read_lines(file = input_file)

  start <- stringr::str_which(data, "Time\tT") # This string delimits the start of the raw data

  end <- stringr::str_which(data, "Results") # The "Results" Field delimits the end of the raw data (one space before it)

  rawdata <- data[start:(end - 2)]

  rawdata_names <- rawdata[1] |>
    stringr::str_split(pattern = "\\t", simplify = TRUE) |>
    stringr::str_replace("T\xb0 OD600:600", replacement = "Temperature")


  rawdata_tidy <- rawdata[-1] |>
    tidyr::as_tibble() |>
    tidyr::separate_wider_delim(cols = value, names = rawdata_names, delim = "\t") |>
    tidyr::pivot_longer(cols = -c(Time, Temperature), names_to = "Well", values_to = "Absorbance (A)") |>
    tidyr::separate_wider_regex(Well, patterns = c(Row = "\\w", Column = "\\d")) |>
    dplyr::mutate(
      Plate = 1,
      Time = readr::parse_time(Time, format = "%h:%M:%S"),
      Column = ifelse(stringr::str_length(Column) == 1, glue::glue("0{Column}"), Column),
      Well = glue::glue("{Row}{Column}"),
      `Absorbance (A)` = as.numeric(`Absorbance (A)`),
      Repeat = dplyr::cur_group_id(),
      .by = Time) |>
    dplyr::select(-Temperature, -Row, -Column) |>
    dplyr::relocate(Plate, Repeat, Well, Time, `Absorbance (A)`)

  return(rawdata_tidy)
}

