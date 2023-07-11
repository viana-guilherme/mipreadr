#' Converts a wide 96-well plate layout into a long one
#'
#' @param plate_layout A .xlsx spreadsheet mapping samples to wells in a 96 well plate
#' @return A 2-column tibble mapping samples to their original well
#'

elongateLayout <- function(plate_layout) {

  # Opens the excel map file:
  plate_layout <- suppressMessages(readxl::read_excel(plate_layout)) %>%
    tibble::column_to_rownames("...1")

  # turns the wide map template into a 2-column tibble
  plate_layout_long <- purrr::map_df( .x = 1:nrow(plate_layout), ~ {
                                          plate_layout[.x,] %>%
                                            tidyr::pivot_longer(cols = 1:ncol(plate_layout),
                                                                values_to = "Samples",
                                                                names_to = "Wells") %>%
                                            dplyr::mutate(Wells = paste0(rownames(plate_layout)[.x],
                                                                         Wells) %>%
                                            stringr::str_remove_all('\"')) %>%
                                            return()
                                          })

  return(stats::na.omit(plate_layout_long))
}


