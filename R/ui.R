# ui file for hds532 application

ui <- navbarPage(
  title = "HDS532 - Data Visualization",
  header = tagList(
    useShinydashboard(),
    useSweetAlert()
  ),
  tabPanel("PA State and County Cases",
           fluidRow("PA State and Country Cases"),
           fluidRow(valueBoxOutput('currentPACases'),
                    ),
           fluidRow(h1("Clickable map here maybe?")),
           fluidRow(h1("Something after a map click here"))),
  tabPanel("PA State and County Vaccination Data"),
  tabPanel("Some sort of analysis tab")
)