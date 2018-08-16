output$FS_LMMPage = shiny::renderUI({
  shiny::fluidPage(
    shiny::fluidRow(
      shiny::HTML("<br><p style='color: #626262; font-size: 12px;'> Use the 'Search' field to provide expressions that subsets the rows of
           the table with shiny::a logical expression ['COLUMN' 'expression' 'value'], ex. Age>20 for Age over 20. See 'Help' tab for more information. <br>"),
      shiny::HTML("<br><b>Base columns are: </b> fsid fsid-base time [FS_LMM specific columns created by the script, necessary for FS_LMM to run]")),

    # Choices ------
    shiny::fluidRow(
      shiny::column(4,shiny::wellPanel(
        shiny::HTML("<br>"),
        shiny::uiOutput('chooseGrouping', inline=T),
        shiny::HTML("<p style='color: #626262; font-size: 12px;'> (i.e. non-numeric covariates, comma separated)-</p><br><br>"),

        shiny::uiOutput('chooseNumeric', inline=T),
        shiny::HTML("<p style='color: #626262; font-size: 12px;'> (i.e. numeric variables of interest, comma separated).</p>"),
        style="background-color: #c6e7f2; border-color: #66bfdd;")),

      shiny::column(4,
        shiny::uiOutput("optsFS")),

    ),

    # Go button ------
    shiny::fluidRow(
      shiny::column(12,shiny::wellPanel(
        shiny::actionButton(inputId = "goClick_FS_LMM", label="Go!", width='80%',
                            style="color: #626262; background-color: #dcdcdc; border-color: #626262; margin: 0px 20px 2px 20px"),
        style="background-color: transparent; border-color: transparent;"))
    ),

    # Display the output and download options -----
    shiny::fluidRow(
      shiny::h4("Table"),
      shiny::fluidRow(shiny::wellPanel(
        shiny::downloadButton("download_FS_LMM", label="Download table"),
        style="background-color: transparent; border-color: transparent; margin-bottom: 2%;")),
      shiny::wellPanel(shiny::p(shiny::textOutput("N_FS_LMM")),
                       shiny::HTML("<p align='center' style='color: #626262; font-size: 12px;'> Random sample of the subsetted data. Scroll sideways to see all columns"),
                       shiny::fluidRow(DT::dataTableOutput("table_FS_LMM") %>% shinycssloaders::withSpinner(type = 5, color = "#66bfdd")))
    )
  )
})

# FS_LMM variables ----------

output$chooseGrouping = shiny::renderUI({
  CLASS = unlist(lapply(subDATA(),class))
  opts = names(subDATA())[grep("factor|character", CLASS)]
  shiny::selectizeInput('choiceGrouping', 'Select grouping variables', opts, selected = character(0),
                        multiple = T, options = list(placeholder = 'Type to start selecting'), width="100%")
})

output$chooseNumeric = shiny::renderUI({
  CLASS = unlist(lapply(subDATA(),class))
  opts = names(subDATA())[grep("numeric|integer", CLASS)]
  # The options are dynamically generated on the server
  shiny::selectizeInput('choiceNumeric', 'Select numeric covariates', opts, selected = character(0),
                        multiple = T, options = list(placeholder = 'Type to start selecting'), width="100%")
})


output$optsFS = shiny::renderUI({
  if(input$actionDoubles == "asis"){
    shiny::wellPanel(
      shiny::radioButtons("keepFS", label="Which data from double/triple scans to keep",
                          choices=c("long", "ousAvanto","ousSkyra","ousPrisma"), inline=T),
      shiny::uiOutput("keepFSHelp")
    )
  }else{
    shiny::wellPanel(
      shiny::radioButtons("keepFS", label="Which data from double/triple scans to keep",
                          choices=c("long", "ousAvanto","ousSkyra","ousPrisma"), inline=T),
      shiny::uiOutput("keepFSHelp")
    )
  }
})





FS_Table = shiny::eventReactive(input$goClick_FS_LMM, {
  if(purrr::is_empty(input$choiceGrouping) | purrr::is_empty(input$choiceNumeric)){
    tmp = MOAS::fs_lmm(data=subDATA(), grouping.var="Sex",numeric.var="Age", missing.action=input$actionFS,keep=input$keepFS)
  }else{
    tmp = MOAS::fs_lmm(data=subDATA(),
                       grouping.var=input$choiceGrouping,
                       numeric.var=input$choiceNumeric,
                       missing.action=input$actionFS,
                       keep=input$keepFS)
  }
  return(tmp)
})

output$actionFSHelp = shiny::renderUI({
  help = switch(input$actionFS,
                Mean   = "Replace missing values with the mean of the participant. Keeps as much available data as possible.",
                Delete = "Delete all missing values from the dataset.",
                First  = "Replace all values with the first recorded instance for the participant. Circumvent change-change relationship.",
                All    = "Replace all values with the mean of the participant. Assumes a stable trait."
  )
  shiny::helpText(help)
})

output$keepFSHelp = shiny::renderUI({
  help = switch(input$keepFS,
                long      = "Keeping data from scanner with most data from double/triple scanned. Retains higher scanner consistency.",
                ousAvanto = "Keeping 'ousAvanto' from double/triple scanned",
                ousSkyra  = "Keeping 'ousSkyra' from double/triple scanned",
                ousPrisma = "Keeping 'ousPrisma' from triple scanned, double scanned Avanto/Skyra removed from file."
  )
  shiny::helpText(help)
})

output$table_FS_LMM = DT::renderDataTable(options = list(scrollX = TRUE, pageLength = 25), {
  FS_Table()
})

output$N_FS_LMM = shiny::renderText({
  N=nrow(FS_Table())
  if(purrr::is_empty(N)){
    paste("Please select both Grouping and Numeric variables for output.")
  }else{
    paste("Number of observations: ", N)
  }
})

output$download_FS_LMM = shiny::downloadHandler(

  filename = function(){
    paste("FS_LMM", Grouping_FS(), Numeric_FS(),format(Sys.Date(),"%d.%m.%Y"),
          "txt", sep=".")
  },

  content = function(file) {
    utils::write.table(x = FS_Table(),
                       file = file,
                       sep=" ", row.names = F, quote = F)
  }
)
