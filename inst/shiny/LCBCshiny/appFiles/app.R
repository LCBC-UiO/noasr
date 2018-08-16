
#Filesize allowance increase (30MB)
options(shiny.maxRequestSize=100*1024^2)

# VARIABLES code --------------

#In case something goes wrong with loading the inDATA, prompt manual upload
if(!exists("inDATA")) {
  InFILE = shiny::eventReactive(input$inDATA,{
    if ( is.null(input$inDATA)) return(NULL)
    inFile = shiny::isolate({input$inDATA })
    tmp = get_file(inFile$datapath)
    return( tmp)
  })

  output$uploadFILE = shiny::renderUI({
    shiny::fileInput("inDATA", "Upload the MOAS.RData file",
              multiple = FALSE,
              accept = c(".RData", ".rda"))
  })

}else{
  InFILE=function(){return(inDATA)}
}

DATA = shiny::reactive({
  InFILE()
})

#Source all the server files
source("appFiles/Pages/Subset_Server.R",    local = T)  # Global subset
source("appFiles/Pages/DataTablePage_UI.R", local = T)  # Tab 1
source("appFiles/Pages/FS_LMMpage_UI.R",    local = T)  # Tab 2
source("appFiles/Pages/PlotPage_UI.R",      local = T)  # Tab 3
source("appFiles/Pages/HelpPage_UI.R",      local = T)  # Tab 4
source("appFiles/Pages/VariablesPage_UI.R", local = T)  # Tab 5

