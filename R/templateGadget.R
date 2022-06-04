
templateGadget <- function(template) {

  column_names <- c(NULL, c(paste0("0", 1:9), 10, 11, 12))
  row_names <- c(NULL, LETTERS[1:8])

  out_table <- matrix(data = "",nrow = 8, ncol = 12) |>
    magrittr::set_colnames(paste0('"',column_names,'"')) |>
    magrittr::set_rownames(row_names)

  rhandsontable::rhandsontable(out_table, width = 9000, height = 300)

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Template Editor")
  )

  server <- function(input, output, session){}

  shiny::runGadget(shiny::shinyApp(ui, server), viewer = shiny::paneViewer())
}
