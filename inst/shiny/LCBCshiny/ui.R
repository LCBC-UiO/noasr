### UI specification -------------
ui=shiny::fluidPage(
  title="LCBC ShinyApp",
  shiny::tags$head(shiny::tags$link(rel="shortcut icon", href="www/Favicon.ico")),
  shiny::tags$style(shiny::HTML(" .tabbable {	position: relative; overflow: hidden;}
                    .tabbable > .nav > li[class=active]    > a {font-weight: bold; border: none; border-bottom: 5px SOLID #008cc8;}
                    .tabbable > .nav > li > a {color: #5a5a5a; font-weight: bold; margin: 0px 2em 0px 2em;}
                  ")),

  shiny::HTML("<div id='over' style='position:relative; margin: 5% 7% 5% 3%;' width:100% align=center>
         <img src='Logo.png' class='img-responsive' align='center' width='100%' height='auto' style='max-width:600px;'/><br><br>"),
  shiny::h4("Mother of All Spreadsheets", align="center", style="color: #626262; font-family: 'Arial'"),
  shiny::HTML("<p align='center' style='color: #626262;'>This application will help you search and explore relevant information in the MOAS<br><br></p>"),
  uiOutput('uploadFILE'),

    shiny::fluidRow(
      shiny::HTML("<br><p style='color: #626262; font-size: 12px;'> Use the 'Search' field to provide expressions that subsets the rows of
           the table with a logical expression ['COLUMN' 'expression' 'value'], ex. Age>20 for Age over 20. See 'Help' tab for more information <br>"),
      shiny::HTML("<br><b>Base columns are: </b>"), paste(BaseCols , collapse = "  ")
      ),

    shiny::fluidRow(
      shiny::column(4,shiny::wellPanel(
        shiny::textInput(inputId="search_DATA", label="Type an expression to reduce rows"),
        shiny::HTML("<p style='color: #626262; font-size: 12px;'> See the 'Help' tab for extra help in searching"),
        shiny::radioButtons("actionDoubles", label="Action for double/triple scan data",
                            choices=c("Keep all"="asis", "Scanner with most data"="long","ousAvanto","ousSkyra","ousPrisma")),
        style="background-color: #e6efc9; border-color: #c5da84;")
      ),

      shiny::column(4,shiny::wellPanel(
        shiny::HTML("<br>"),
        shiny::uiOutput('ColSelect_SubData', inline=T),
        shiny::checkboxInput("tickSelect", label="Remove rows with empty observations in the above columns", value = FALSE, width = NULL),
        shiny::textInput('ColSearch_SubData', label = "Add all columns containing:", value = "", placeholder = "Comma separated list of strings"),
        style="background-color: #c6e7f2; border-color: #66bfdd;")
      ),
      shiny::column(4, shiny::wellPanel(
        shiny::uiOutput('widenTable', inline=T),
        shiny::uiOutput("actionTableHelp"))
      ),
      style="margin: 2% 0px 0px 0px"),

    shiny::fluidRow(
      shiny::column(12,
             shiny::wellPanel(shiny::actionButton(inputId = "goClick_DATA", label="Go!", width='80%',
                                    style="color: #626262; background-color: #dcdcdc; border-color: #626262; margin: 0px 20px 2px 20px"),
                       shiny::HTML("<br><br><br>"),
                       style="background-color: transparent; border-color: transparent;"))
    ),
  shiny::tabsetPanel(
    shiny::tabPanel("Data table", shiny::uiOutput('DataTablePage')),
    shiny::tabPanel("FS LMM", shiny::uiOutput('FS_LMMPage')),
    shiny::tabPanel("Plot", shiny::uiOutput('PlotPage')),
    shiny::tabPanel("Variables", shiny::uiOutput('VariablesPage')),
    shiny::tabPanel("Help", shiny::uiOutput('HelpPage'))
  ),

  shiny::fluidRow(
    shiny::hr(),
    shiny::HTML("<p align='center' style='color: #626262; font-size: 12px;'> Created by Athanasia Monika Mowinckel </p>")
  ),

  shiny::HTML("</div>")
)



