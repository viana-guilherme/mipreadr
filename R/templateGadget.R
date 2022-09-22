templateGadget <- function(layout_path, layout_name = "") {

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar(glue::glue("mipreadr Template Editor - {layout_name}")),
    shiny::fillPage(
    miniUI::miniContentPanel(scrollable = FALSE, padding = 10,
    rhandsontable::rHandsontableOutput("rTable")))
  )

  server <- function(input, output, session){

    plate_layout <- suppressMessages(readxl::read_excel(layout_path, col_types = "text")) |>
      tibble::column_to_rownames("...1")

    output$rTable <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(plate_layout, width = "100%", stretchW = "all", stretchH = "all")
    })

    shiny::observeEvent(input$done, {

      out_data <- rhandsontable::hot_to_r(input$rTable) |>
        dplyr::mutate(" " = rownames(plate_layout)) |>
        dplyr::relocate(.data[[" "]])

      writexl::write_xlsx(x = out_data,
                          format_headers = FALSE,
                          path = layout_path)
      shiny::stopApp()
    })

    shiny::observeEvent(input$cancel, {
      shiny::stopApp()
    })

  }

    suppressMessages(shiny::runGadget(shiny::shinyApp(ui, server),
                   viewer = shiny::paneViewer(minHeight = 300)))
}
