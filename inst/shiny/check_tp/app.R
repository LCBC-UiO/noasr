
library(shiny)
library(dplyr)
library(stringr)
library(MOAS)
options(shiny.maxRequestSize=100*1024^2)

get_file = function(PATH){
  # load the file into new environment and get it from there
  e = new.env()
  name = load(PATH, envir = e)

  return( e[[name]])
}


# Define UI for application that draws a histogram
### UI specification -------------
ui <- fluidPage(
  title="LCBC check subject timepoint",
  tags$head(tags$link(rel="shortcut icon", href="www/Favicon.ico")),

  HTML("<div id='over' style='position:relative; margin: 5% 7% 5% 3%;' width:100% align=center>
              <img src='Logo.png' class='img-responsive' align='center' width='100%' height='auto' style='max-width:600px;'/><br><br>"),
  h4("Check subject timepoint", align="center", style="color: #626262; font-family: 'Arial'"),
  HTML("<p align='center' style='color: #626262;'>This application will help you find a MOAS subject timepoint<br><br></p>"),
  uiOutput('uploadFILE'),

  HTML("<br>"),
  fluidRow(
    column(6,
           helpText("Start typing a number, the drop-down list is not exhaustive."),
           uiOutput('ID_choice', inline=T)),
    column(6,
           wellPanel(
             actionButton(
               inputId = "go", label="Go!", width='80%',
               style="color: #626262; background-color: #dcdcdc;
               border-color: #626262; margin: 0px 20px 2px 20px"),
             HTML("<br><br><br>"),
             style="background-color: transparent; border-color: transparent;"))
  ),

  fluidRow(wellPanel(h3(textOutput("options")), style="background-color: #c6e7f2; border-color: #66bfdd;")),
  HTML("<br>"),
  fluidRow(dataTableOutput("tp_options")),
  fluidRow(
    hr(),
    HTML("<p align='center' style='color: #626262; font-size: 12px;'> Created by Athanasia Monika Mowinckel </p>")
  ),

  HTML("</div>")
)


# Define server logic required to draw a histogram
server <- function(input, output) {

  if(!exists("inDATA")) {
    InFILE = eventReactive(input$inDATA,{
      if ( is.null(input$inDATA)) return(NULL)
      inFile = isolate({input$inDATA })
      tmp = get_file(inFile$datapath)
      return(tmp)
    })

    output$uploadFILE = renderUI({
      fileInput("inDATA", "Upload the MOAS.RData file",
                multiple = FALSE,
                accept = c(".RData", ".rda"))
    })

  }else{
    InFILE=function(){return(inDATA)}
  }


  output$ID_choice = renderUI({
    # The options are dynamically generated on the server
    opts = unique(InFILE()$CrossProject_ID)
    selectizeInput('ID_selected', 'Choose ID', opts, selected = character(0),
                   multiple = F, options = list(placeholder = 'Type to start selecting'), width="100%")
  })


  output$tp_options = renderDataTable({
    check_tp(input$ID_selected, InFILE())
  })

  output$options = renderPrint({
    t <- check_tp(input$ID_selected, InFILE())
  })

}

# Run the application
shinyApp(ui = ui, server = server)

