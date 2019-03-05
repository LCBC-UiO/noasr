output$DataTablePage = shiny::renderUI({
  shiny::fluidPage(
    shiny::fluidRow(
      shiny::h4("Data exerpt"),
      # uiOutput('SumTable_DATA', inline=T),
      shiny::fluidRow(shiny::wellPanel(
        shiny::column(7,
               shiny::selectInput(inputId="downloadType_subDATA",
                           label="Choose file type",
                           choices=DownloadOptionsTable,
                           width='40%')),
        shiny::column(5,
               shiny::downloadButton("download_subDATA",
                              label="Download table")),
        style="background-color: transparent; border-color: transparent; margin-bottom: 2%;")),
      shiny::wellPanel(shiny::p(shiny::textOutput("N_DATA")),
                shiny::fluidRow(DT::dataTableOutput("table_DATA") %>%
                                  shinycssloaders::withSpinner(type = 5, color = "#66bfdd")))
    )
  )
})


output$table_DATA = DT::renderDataTable({
  NUMS = apply(subDATA_structured(),2,
               function(x) (as.numeric(x)%%1 > 0 ) %>% any)
  NUMS = (NUMS %in% TRUE) %>% which()

  subDATA_structured() %>%
    DT::datatable(options = list(scrollX = TRUE,pageLength = 25)) %>%
    DT::formatStyle(columns = c(1:ncol(subDATA_structured())), 'text-align' = 'center') %>%
    DT::formatRound(columns=NUMS, digits=2)
})

output$N_DATA = shiny::renderText({
  paste("Number of observations: ", nrow(subDATA_structured()))
})

output$download_subDATA = shiny::downloadHandler(

  filename = function(){
    EXT=ifelse(input$downloadType_subDATA =="csv2","csv",input$downloadType_subDATA)

    paste("Table", input$search_DATA, ExCols(),format(Sys.Date(),"%d.%m.%Y"),
          EXT, sep=".")
  },

  content = function(file) {
    rio::export(x = subDATA_structured(),
                file = file,
                format = input$downloadType_subDATA)
  }
)
