# ui file for hds532 application

ui <- navbarPage(
  title = "HDS532 - Data Visualization",
  header = tagList(
    useShinydashboard(),
    useSweetAlert()
  ),
  tabPanel("PA State and County Cases",
           fluidRow(h1("PA State Cases")),
           fluidRow(valueBoxOutput('currentPACases'),
                    valueBoxOutput('pa7DayRolling'),
                    valueBoxOutput('totalPaCases')),
           fluidRow(plotlyOutput('paCasePlot')),
           fluidRow(br(),
                    h1("PA County Cases")),
           fluidRow(column(5),
                    column(2,uiOutput('casesCountySelection')),
                    column(5)
                    ),
           fluidRow(plotlyOutput('countyCasePlot'))),
  tabPanel("PA State and County Vaccination Data",
           fluidRow(h1("PA State Vaccination Details")),
           fluidRow('Some Charts here')),
  tabPanel("Some sort of analysis tab"),
  footer = "App Version 0.1.0"
)