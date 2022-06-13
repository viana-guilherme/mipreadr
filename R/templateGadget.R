templateGadget <- function(template_path, template_name = "") {

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar(glue::glue("x3 Template Editor - {template_name}")),
    shiny::fillPage(
    miniUI::miniContentPanel(scrollable = FALSE, padding = 10,
    rhandsontable::rHandsontableOutput("rTable")))
  )

  server <- function(input, output, session){

    plate_layout <- suppressMessages(readxl::read_excel(template_path, col_types = "text")) |>
      tibble::column_to_rownames("...1")

    output$rTable <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(plate_layout, width = "100%", stretchW = "all", stretchH = "all")
    })

    observeEvent(input$done, {

      out_data <- rhandsontable::hot_to_r(input$rTable) |>
        dplyr::mutate(" " = rownames(plate_layout)) |>
        dplyr::relocate(` `)

      writexl::write_xlsx(x = out_data,
                          format_headers = FALSE,
                          path = template_path)
      stopApp()
    })

    observeEvent(input$cancel, {
      stopApp()
    })

  }

    suppressMessages(shiny::runGadget(shiny::shinyApp(ui, server),
                   viewer = shiny::paneViewer(minHeight = 300)))
}
