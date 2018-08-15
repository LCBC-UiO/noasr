### UI specification -------------
ui=fluidPage(
  title="LCBC ShinyApp",
  tags$head(tags$link(rel="shortcut icon", href="Favicon.ico")),
  tags$style(HTML(" .tabbable {	position: relative; overflow: hidden;}
                    .tabbable > .nav > li[class=active]    > a {font-weight: bold; border: none; border-bottom: 5px SOLID #008cc8;}
                    .tabbable > .nav > li > a {color: #5a5a5a; font-weight: bold; margin: 0px 2em 0px 2em;}
                  ")),
  
  HTML("<div id='over' style='position:relative; margin: 5% 7% 5% 3%;' width:100% align=center>
         <img src='Logo.png' class='img-responsive' align='center' width='100%' height='auto' style='max-width:600px;'/><br><br>"),
  h4("Mother of All Spreadsheets", align="center", style="color: #626262; font-family: 'Arial'"),
  HTML("<p align='center' style='color: #626262;'>This application will help you search and explore relevant information in the MOAS<br><br></p>"),
  uiOutput('uploadFILE'),
    
    fluidRow(
      HTML("<br><p style='color: #626262; font-size: 12px;'> Use the 'Search' field to provide expressions that subsets the rows of 
           the table with a logical expression ['COLUMN' 'expression' 'value'], ex. Age>20 for Age over 20. See 'Help' tab for more information <br>"),
      HTML("<br><b>Base columns are: </b>"), paste(BaseCols , collapse = "  ")
      ),
    
    fluidRow(
      column(4,wellPanel(
        textInput(inputId="search_DATA", label="Type an expression to reduce rows"),
        HTML("<p style='color: #626262; font-size: 12px;'> See the 'Help' tab for extra help in searching"),
        style="background-color: #e6efc9; border-color: #c5da84;")
      ),
      
      column(4,wellPanel( 
        HTML("<br>"),
        uiOutput('ColSelect_SubData', inline=T),
        checkboxInput("tickSelect", label="Remove rows with empty observations in the above columns", value = FALSE, width = NULL),
        textInput('ColSearch_SubData', label = "Add all columns containing:", value = "", placeholder = "Comma separated list of strings"),
        style="background-color: #c6e7f2; border-color: #66bfdd;")
      ),
      column(4, wellPanel(
        radioButtons("actionTable", label="Table format",
                     choices=WidenChoices),
        uiOutput("actionTableHelp"))
      ),
      style="margin: 2% 0px 0px 0px"),
    
    fluidRow(
      column(12,  
             wellPanel(actionButton(inputId = "goClick_DATA", label="Go!", width='80%',
                                    style="color: #626262; background-color: #dcdcdc; border-color: #626262; margin: 0px 20px 2px 20px"),
                       HTML("<br><br><br>"),
                       style="background-color: transparent; border-color: transparent;"))
    ),
  tabsetPanel(
    tabPanel("Data table", uiOutput('DataTablePage')),
    tabPanel("FS LMM", uiOutput('FS_LMMPage')),
    tabPanel("Plot", uiOutput('PlotPage')),
    tabPanel("Variables", uiOutput('VariablesPage')),
    tabPanel("Help", uiOutput('HelpPage'))
  ),
  
  fluidRow(
    hr(),
    HTML("<p align='center' style='color: #626262; font-size: 12px;'> Created by Athanasia Monika Mowinckel </p>")
  ),
  
  HTML("</div>")
)



