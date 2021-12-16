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
           fluidRow(h1("PA State Vaccination Details")),
           fluidRow(valueBoxOutput('vaccineAtleast1Dose'),
                    valueBoxOutput('totalVaccineCoverage'),
                    valueBoxOutput("totalVaccineAdditioanlCoverage")),
           fluidRow(column(10),
                    column(2,
                           fluidRow(
                             p("Turn on CumSum Line:"),
                             switchInput('turnOnLine', onLabel = "Yes", offLabel = "No")
                           )
                    )
                    ),
                           
           fluidRow(plotlyOutput('paVaccinePlot')),
           fluidRow(column(10),
                    column(2,
                           downloadButton('downloadPAVaccineData', "Download PA Vaccine data"))),
           fluidRow(br(),
                    h1("PA County Vaccine Data")),
           fluidRow(column(5),
                    column(2,uiOutput('vaccinesCountrySelection')),
                    column(5)),
           fluidRow(plotlyOutput('countyVaccinePlot')),
           fluidRow(column(6),
                    column(4,
                           splitLayout(cellWidths = c("45%", "45%"),
                                       downloadButton('downloadPACountyVaccineData', "Download Selected County data"),
                                       downloadButton('downloadAllPACountyVaccineData', "Download All County data"))))
           ),
  tabPanel("Analysis Tab",
           fluidRow(h1("Analysis in 3 Steps")),
           tabsetPanel(
             tabPanel("Data Normalization",
                      h3("The fist step in our analysis is to normalize the data.  I have decided to do a min/max range where the minimum value is scaled to 0 and the maximum 1.  This scaling will allow us to compare the cases and vaccinated data a little better.  As you can see, while the numbers on the y-axis are different, the shape of the data does not change."),
                      fluidRow(plotlyOutput('normalFacet'))),
             tabPanel("Combining Normalized Data",
                      h3("The next step in our analysis is to combine the normalized data.  The top plots show the individual data and the bottom plot the overlay.  The combined (bottom) plot is a 'dodged' column plot, the dark spots are where both data is present."),
                      fluidRow(plotlyOutput('combinedPlotFacet')),
                      fluidRow(plotlyOutput('combinedPlot'))),
             tabPanel("Annotating Plots",
                      h3("Finally, we can draw some insights from our final, combined plot.
                        - Once vaccinations started, the case count can be seen to decrease (black line)
                        - Once a majority have been vaccinated, cases dropped to an all time low (black box)
                        - As of today, cases on the rise again, possibly indicating vaccine effectiveness is dropping (red box)"),
                      fluidRow(plotlyOutput('annotationPlot')))
             
           )
           ),
  footer = "App Version 1.0.1"
)