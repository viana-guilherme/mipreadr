#' Generate a 96-well map template
#' @param layout_name A string specifying the name for the map template.
#' @param interactive Use interactive mode to design template
#' @export
#'

createLayout <- function(layout_name, interactive = FALSE) {

  column_names <- c(NULL, c(paste0("0", 1:9), 10, 11, 12))
  row_names <- c(NULL, LETTERS[1:8])

  out_table <- matrix(data = " ",nrow = 8, ncol = 12) |>
                magrittr::set_colnames(paste0('"',column_names,'"')) |>
                magrittr::set_rownames(row_names) |>
                tibble::as_tibble(rownames = " ")

  out_path <- file.path(here::here(),paste0(layout_name,".xlsx"))

  writexl::write_xlsx(x = out_table,
                      format_headers = FALSE,
                      path = out_path)

  message(glue::glue("Template for plate {layout_name} created in {here::here()}!"))

  if(interactive) { templateGadget(out_path, layout_name = layout_name) }

 }



