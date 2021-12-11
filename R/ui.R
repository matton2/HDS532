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
           fluidRow(column(10),
                    column(2,
                           downloadButton('downloadPAStateCaseData', "Download PA Case data"))),
           fluidRow(br(),
                    h1("PA County Cases")),
           fluidRow(column(5),
                    column(2,uiOutput('casesCountySelection')),
                    column(5)
                    ),
           fluidRow(plotlyOutput('countyCasePlot')),
           fluidRow(column(6),
                    column(4,
                           splitLayout(cellWidths = c("45%", "45%"),
                           downloadButton('downloadPACountyCaseData', "Download Selected County data"),
                          downloadButton('downloadAllPACountyCaseData', "Download All County data"))))
           ),
  tabPanel("PA State and County Vaccination Data",
           fluidRow(valueBoxOutput('vaccineAtleast1Dose'),
                    valueBoxOutput('totalVaccineCoverage'),
                    valueBoxOutput("totalVaccineAdditioanlCoverage")),
           fluidRow(h1("PA State Vaccination Details")),
           fluidRow(column(10),
                    column(2,
                           fluidRow(
                             p("Turn on CumSum Line:"),
                             switchInput('turnOnLine', onLabel = "Yes", offLabel = "No")
                           )
                    )
                    ),
                           
           fluidRow(plotlyOutput('paVaccinePlot')),
           fluidRow(br(),
                    h1("PA County Vaccine Data")),
           fluidRow(column(5),
                    column(2,uiOutput('vaccinesCountrySelection')),
                    column(5)),
           fluidRow(plotlyOutput('countyVaccinePlot'))
           ),
  tabPanel("Some sort of analysis tab"),
  footer = "App Version 0.2.0"
)