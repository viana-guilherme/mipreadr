#' Generate a 96-well map template
#' @param layout_name A string specifying the name for the map template.
#' @param interactive Use interactive mode to design template
#' @export
#'

createLayout <- function(layout_name, interactive = FALSE, platetype = "96") {

  # the default is a 96-well plate
  ncol <- 12
  nrow <- 8

  if (platetype == "48") {
    ncol <- 8
    nrow <- 6
  }

  column_names <- 1:ncol |>
                    as.character() |>
                    purrr::map_chr(~ dplyr::if_else(stringr::str_length(.) == 1, glue::glue("0{.x}"), .x))

  row_names <- c(NULL, LETTERS[1:nrow])

  out_table <- matrix(data = " ",nrow = nrow, ncol = ncol) |>
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



