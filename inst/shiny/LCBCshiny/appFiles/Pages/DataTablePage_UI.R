output$DataTablePage = renderUI({
  fluidPage(
    fluidRow(
      h4("Data exerpt"),
      # uiOutput('SumTable_DATA', inline=T),
      fluidRow(wellPanel(
        column(7,
               selectInput(inputId="downloadType_subDATA", label="Choose file type", choices=DownloadOptionsTable, width='40%')),
        column(5,
               downloadButton("download_subDATA", label="Download table")),
        style="background-color: transparent; border-color: transparent; margin-bottom: 2%;")),
      HTML("<p align='center' style='color: #626262; font-size: 12px;'> Random sample of the subsetted data. Scroll sideways to see all columns"),
      wellPanel(p(textOutput("N_DATA")),
                fluidRow(dataTableOutput("table_DATA") %>% withSpinner(type = 5, color = "#66bfdd")))
    )
  )
})


output$actionTableHelp = renderUI({
  help = switch(input$actionTable,
                "none" = "",
                "Site_Name"     = "one row per scanning and project wave",
                "s2w"           = "one row per participant",
                "t2w"           = "one row per participant"
  )
  helpText(help) 
})


output$table_DATA = renderDataTable({
  n=sample(1:(nrow(subDATA_structured())-24),1)
  NUMS = apply(subDATA_structured(),2,
               function(x) (as.numeric(x)%%1 > 0 ) %>% any)
  NUMS = (NUMS %in% TRUE) %>% which()
  
  subDATA_structured() %>% #[n:(n+24),] %>% 
    datatable(options = list(scrollX = TRUE,pageLength = 25)) %>% 
    formatStyle(columns = c(1:ncol(subDATA_structured())), 'text-align' = 'center') %>% 
    formatRound(columns=NUMS, digits=2)
})

output$N_DATA = renderText({
  paste("Number of observations: ", nrow(subDATA_structured()))
})

output$download_subDATA = downloadHandler(
  
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
