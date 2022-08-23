#' Create a snapshot of a specific time point for a plate
#' @param input_file A Victor X3 output file (in format .txt)
#' @param variable A variable for visualization
#' @param time_point Which of the time points (Repeat) to visualize?
#' @param log_scale (logical) Should the output plot be log-scaled?
#' @export
#'

checkPlate <- function(input_file, variable, time_point, log_scale = TRUE) {

plot_scale <- dplyr::if_else(log_scale == TRUE, "log10", "identity")

input <- readPlateInput(input_file)
varName <- stringr::str_subset(colnames(input), variable)


input %>%
  dplyr::select(Repeat, Well, tidyselect::all_of(varName)) %>%
  dplyr::filter(Repeat == time_point) %>%
  dplyr::mutate(Col = stringr::str_extract(Well, pattern = "\\w"),
                Row = stringr::str_extract(Well, pattern = "\\d{2}")) %>%
  dplyr::relocate(Col, Row, .after = Well) %>%
  ggplot2::ggplot(ggplot2::aes(x = Row, y = Col, label = .data[[varName]], fill = .data[[varName]])) +
  ggplot2::geom_tile() +
  ggplot2::scale_fill_gradientn(colours = grDevices::hcl.colors(10, palette = "Spectral", rev = TRUE), trans = plot_scale) +
  ggplot2::scale_y_discrete(limits = rev) +
  ggplot2::scale_x_discrete(position = "top") +
  ggplot2::labs(x = "", y = "") +
  ggplot2::geom_text() +
  # ggplot2::ggtitle(glue::glue("Checking {parameter} for sample {sample}")) +
  ggplot2::theme_minimal()



}
