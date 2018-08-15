output$FS_LMMPage = renderUI({
  fluidPage(          
    fluidRow(
      HTML("<br><p style='color: #626262; font-size: 12px;'> Use the 'Search' field to provide expressions that subsets the rows of 
           the table with a logical expression ['COLUMN' 'expression' 'value'], ex. Age>20 for Age over 20. See 'Help' tab for more information. <br>"),
      HTML("<br><b>Base columns are: </b> fsid fsid-base time [FS_LMM specific columns created by the script, necessary for FS_LMM to run]")),
    
    # Choices ------
    fluidRow(
      column(4,wellPanel( 
        HTML("<br>"),
        uiOutput('chooseGrouping', inline=T),
        HTML("<p style='color: #626262; font-size: 12px;'> (i.e. non-numeric covariates, comma separated)-</p><br><br>"),
        
        uiOutput('chooseNumeric', inline=T),
        HTML("<p style='color: #626262; font-size: 12px;'> (i.e. numeric variables of interest, comma separated).</p>"),
        style="background-color: #c6e7f2; border-color: #66bfdd;")),
      
      column(4,wellPanel(
        radioButtons("actionFS", label="Action to take for values in 'Numeric' columns",
                     choices=c("Mean","All","First","Delete"), inline=T),
        uiOutput("actionFSHelp"))),
      
      column(4,wellPanel(
        radioButtons("keepFS", label="Which data from double/triple scans to keep",
                     choices=c("long", "ousAvanto","ousSkyra","ousPrisma"), inline=T),
        uiOutput("keepFSHelp")))
    ),

    # Go button ------
    fluidRow(
      column(12,wellPanel(
        actionButton(inputId = "goClick_FS_LMM", label="Go!", width='80%',
                     style="color: #626262; background-color: #dcdcdc; border-color: #626262; margin: 0px 20px 2px 20px"),
        style="background-color: transparent; border-color: transparent;"))
    ),
    
    # Display the output and download options -----
    fluidRow(
      h4("Table"),
      fluidRow(wellPanel(
        downloadButton("download_FS_LMM", label="Download table"),
        style="background-color: transparent; border-color: transparent; margin-bottom: 2%;")),
      wellPanel(p(textOutput("N_FS_LMM")),
                HTML("<p align='center' style='color: #626262; font-size: 12px;'> Random sample of the subsetted data. Scroll sideways to see all columns"),
                fluidRow(dataTableOutput("table_FS_LMM") %>% withSpinner(type = 5, color = "#66bfdd")))
    )
    )
})

# FS_LMM variables ----------

output$chooseGrouping = renderUI({
  CLASS = unlist(lapply(subDATA(),class))
  opts = names(subDATA())[grep("factor|character", CLASS)]
  selectizeInput('choiceGrouping', 'Select grouping variables', opts, selected = character(0), 
                 multiple = T, options = list(placeholder = 'Type to start selecting'), width="100%")
})

output$chooseNumeric = renderUI({
  CLASS = unlist(lapply(subDATA(),class))
  opts = names(subDATA())[grep("numeric|integer", CLASS)]
  # The options are dynamically generated on the server
  selectizeInput('choiceNumeric', 'Select numeric covariates', opts, selected = character(0), 
                 multiple = T, options = list(placeholder = 'Type to start selecting'), width="100%")
})

FS_Table = eventReactive(input$goClick_FS_LMM, {
  if(is_empty(input$choiceGrouping) | is_empty(input$choiceNumeric)){
    tmp = FS_LMM(subDATA(), "Sex", "Age", input$actionFS,keep=input$keepFS)
  }else{
    tmp = FS_LMM(DATA=subDATA(),
                 Grouping=input$choiceGrouping, 
                 Numeric=input$choiceNumeric, 
                 action=input$actionFS,
                 keep=input$keepFS)
  }
  return(tmp) 
})

output$actionFSHelp = renderUI({
  help = switch(input$actionFS,
                Mean   = "Replace missing values with the mean of the participant. Keeps as much available data as possible.",
                Delete = "Delete all missing values from the dataset.",
                First  = "Replace all values with the first recorded instance for the participant. Circumvent change-change relationship.",
                All    = "Replace all values with the mean of the participant. Assumes a stable trait."
  )
  helpText(help) 
})

output$keepFSHelp = renderUI({
  help = switch(input$keepFS,
                long      = "Keeping data from scanner with most data from double/triple scanned. Retains higher scanner consistency.",
                ousAvanto = "Keeping 'ousAvanto' from double/triple scanned",
                ousSkyra  = "Keeping 'ousSkyra' from double/triple scanned",
                ousPrisma = "Keeping 'ousPrisma' from triple scanned, double scanned Avanto/Skyra removed from file."
  )
  helpText(help) 
})

output$table_FS_LMM = renderDataTable(options = list(scrollX = TRUE, pageLength = 25), {
  FS_Table()
})

output$N_FS_LMM = renderText({
  N=nrow(FS_Table())
  if(is_empty(N)){
    paste("Please select both Grouping and Numeric variables for output.")
  }else{
    paste("Number of observations: ", N)
  }
})

output$download_FS_LMM = downloadHandler(
  
  filename = function(){
    paste("FS_LMM", Grouping_FS(), Numeric_FS(),format(Sys.Date(),"%d.%m.%Y"),
          "txt", sep=".")
  },
  
  content = function(file) {
    write.table(x = FS_Table(),
                file = file,
                sep=" ", row.names = F, quote = F)
  }
)
