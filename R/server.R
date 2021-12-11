# server app for HDS532


server <- function(input, output, session) {
  
  progressSweetAlert(
    session = session, id = "apiProgress",
    title = "Getting data from the PA Website, Vaccine data first",
    display_pct = TRUE, value = 0
  )
  
  
  # for this application, I dont think we will need any reactive values but we will see
  # as it turns out, i do need reactive values!
  
  rv <- reactiveValues()
  
  countyVaccineDate <- RSocrata::read.socrata( "https://data.pa.gov/resource/bicw-3gwi.json",
                                     app_token = 'zqKFZgHzagrp74fam4vXZ7aWH') %>% 
    mutate(across(c(3:5), parse_integer),
           date = lubridate::as_date(date))
  
  updateProgressBar(
    session = session,
    id = "apiProgress",
    title = "Getting additional vaccine data",
    value = 20
  )
  
  # api documentation: https://dev.socrata.com/foundry/data.pa.gov/gcnb-epac
  coveredPercent <- RSocrata::read.socrata( "https://data.pa.gov/resource/gcnb-epac.json",
                                            app_token = 'zqKFZgHzagrp74fam4vXZ7aWH') %>% 
    mutate(across(everything(), parse_guess),
           percentPartial = partially_covered/county_population,
           percentFull = fully_covered/county_population,
           percentWithAtleast1Dose = (partially_covered + fully_covered)/county_population)
  
  updateProgressBar(
    session = session,
    id = "apiProgress",
    title = "Getting cases data",
    value = 40
  )
  
  # api documentation: https://dev.socrata.com/foundry/data.pa.gov/j72v-r42c
  casesCount <- RSocrata::read.socrata("https://data.pa.gov/resource/j72v-r42c.json",
                                       app_token = 'zqKFZgHzagrp74fam4vXZ7aWH') %>% 
    mutate(across(c(3:12), parse_number)) %>% 
    arrange(date) %>% 
    group_by(county) %>% 
    mutate(rolling7Day = round(roll_mean(cases, n = 7, align = "right", fill = NA),0),
           date = lubridate::as_date(date))%>% 
    ungroup()
  
  updateProgressBar(
    session = session,
    id = "apiProgress",
    title = "Doing Maths",
    value = 60
  )
  
  paTotalsToday <- casesCount %>% 
    filter(date == max(date)) %>% 
    summarise(date = max(date), 
              todayCases = sum(cases))
  
  paRollingAvg <- casesCount %>% 
    group_by(date) %>% 
    summarise(cases = sum(cases)) %>% 
    mutate(rolling7Day = round(roll_mean(cases, n = 7, align = "right", fill = NA),0),
           totalCases = cumsum(cases),
           date = lubridate::as_date(date))
  
  paVaccineTotal <- coveredPercent %>% 
    summarise(totalPop = sum(county_population),
              totalPartial = sum(partially_covered),
              totalFully = sum(fully_covered),
              totalAdditional = sum(additional_dose1)) %>% 
    mutate(percentFull = totalFully/totalPop,
           percentPartial = totalPartial/totalPop,
           percentAdditional = totalAdditional/totalPop,
           atLeastOneDose = (totalPartial + totalFully + totalAdditional)/totalPop)
  
  paVaccineDates <- countyVaccineDate %>% 
    group_by(date) %>% 
    summarise(partially_covered = sum(partially_covered, na.rm = TRUE),
              fully_covered = sum(fully_covered, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(cumSumPartial = cumsum(partially_covered),
           cumSumFully = cumsum(fully_covered),
           rolling7DayFullyCovered = round(roll_mean(fully_covered, n = 7, align = "right", fill = NA),0))
  
  combinedPAData <- paRollingAvg %>% 
    left_join(paVaccineDates) %>% 
    pivot_longer(cols = -date, names_to = 'measurement') %>% 
    filter(measurement %in% c('rolling7Day', 'rolling7DayFullyCovered'))
  
  
  updateProgressBar(
    session = session,
    id = "apiProgress",
    title = "Making Plots and Tables",
    value = 80
  )
  
  output$currentPACases <- renderValueBox(
    valueBox(
      scales::comma(paTotalsToday$todayCases),
      paste("Cases Count for", paTotalsToday$date),
      icon = icon("biohazard")
    )
  )
  
  output$pa7DayRolling <- renderValueBox(
    valueBox(
      scales::comma(paRollingAvg$rolling7Day[NROW(paRollingAvg)]),
      "Current 7 Day Rolling Average",
      color = "green"
    )
  )
  
  output$totalPaCases <- renderValueBox(
    valueBox(
      scales::comma(sum(paRollingAvg$cases)),
      "Total PA Cases (overall)",
      color = "orange"
    )
  )
  
  output$vaccineAtleast1Dose <- renderValueBox(
    valueBox(
      scales::percent(paVaccineTotal$atLeastOneDose),
      "PA Residents with atleast 1 Dose",
      color = "green"
    )
  )
  
  output$totalVaccineCoverage <- renderValueBox(
    valueBox(
      scales::percent(paVaccineTotal$percentFull),
      "PA Residents Fully Vaccinated (2 Doses)",
      color = "green"
    )
  )
  
  output$totalVaccineAdditioanlCoverage <- renderValueBox(
    valueBox(
      scales::percent(paVaccineTotal$percentAdditional),
      "PA Residents with an Additional Dose",
      color = "purple"
    )
  )
  
  
  output$paCasePlot <- renderPlotly(
    ggplotly(
    ggplot(paRollingAvg, aes(x = date, y = cases)) +
      geom_col() +
      geom_line(aes(x = date, y=rolling7Day), color = 'blue') +
      theme_classic() +
      scale_x_date(date_labels = "%Y %b %d", date_breaks = '3 month') +
      labs(
        x = "",
        y = "Case Count",
        title = "PA State Daily Case Count and 7 Day Rolling Average"
      )
    )
  )
  
  output$casesCountySelection <- renderUI(
    selectizeInput('casesCountyInput', "Select County", choices = sort(unique(casesCount$county)))
  )
  
  output$vaccinesCountrySelection <- renderUI(
    selectizeInput('vaccineCountyInput', "Select County", choices = sort(unique(countyVaccineDate$county)))
  )
  
  output$countyCasePlot <- renderPlotly({
    
    req(input$casesCountyInput)
    
    temp <- casesCount %>% filter(county == input$casesCountyInput)
    
    ggplotly(
      ggplot(temp, aes(x = date, y = cases)) +
        geom_col() +
        geom_line(aes(x = date, y=rolling7Day), color = 'blue') +
        theme_classic() +
        scale_x_date(date_labels = "%Y %b %d", date_breaks = '3 month') +
        labs(
          x = "",
          y = "Case Count",
          title = paste(input$casesCountyInput, "Daily Case Count and 7 Day Rolling Average"
        )
    )
    )
  })
  
  output$countyVaccinePlot <- renderPlotly({
    
    req(input$vaccineCountyInput)
    
    temp <- countyVaccineDate %>% filter(county == input$vaccineCountyInput)
    
    ggplotly(
      ggplot(temp, aes(x = date, y = fully_covered)) +
        geom_col() +
        theme_classic() +
        scale_x_date(date_labels = "%Y %b %d", date_breaks = '3 month') +
        labs(
          x = "",
          y = "Vaccine Count",
          title = paste(input$vaccineCountyInput, "Daily Vaccine Data (Fully Covered)"
          )
        )
    )
  })
  
  # init the basic plot without the geom line
  rv$paVaccinePlot <- ggplot(paVaccineDates, aes(x = date, y = fully_covered)) +
    geom_col() +
    #geom_line(aes(x = date, y=cumSumFully), color = 'blue') +
    theme_classic() +
    scale_x_date(date_labels = "%Y %b %d", date_breaks = '3 month') +
    labs(
      x = "",
      y = "Fully Vaccinated Count",
      title = "PA State Daily Fully Vaccinated and CumSum"
    )
  
  output$paVaccinePlot <- renderPlotly({
    ggplotly(
      rv$paVaccinePlot
    )
  })
    
  # change the plot based on the turnOnLine input
    observeEvent(input$turnOnLine, {
      if(input$turnOnLine == TRUE) {
        rv$paVaccinePlot <- ggplot(paVaccineDates, aes(x = date, y = fully_covered)) +
          geom_col() +
          geom_line(aes(x = date, y=cumSumFully), color = 'blue') +
          theme_classic() +
          scale_x_date(date_labels = "%Y %b %d", date_breaks = '3 month') +
          labs(
            x = "",
            y = "Fully Vaccinated Count",
            title = "PA State Daily Fully Vaccinated and CumSum"
          )
      } else {
        rv$paVaccinePlot <- ggplot(paVaccineDates, aes(x = date, y = fully_covered)) +
          geom_col() +
          #geom_line(aes(x = date, y=cumSumFully), color = 'blue') +
          theme_classic() +
          scale_x_date(date_labels = "%Y %b %d", date_breaks = '3 month') +
          labs(
            x = "",
            y = "Fully Vaccinated Count",
            title = "PA State Daily Fully Vaccinated and CumSum"
          )
      }
      
    }, ignoreInit = TRUE)
  
  # the code below will be used to make a map at some point in the future
  # paMap <- map('state', region = 'penn')
  # 
  # paCounty <- readLines("./PaCounty2021_06.geojson") %>% paste(collapse = "\n")
  # 
  # output$paCasesMap <- renderLeaflet(
  #   leaflet(paMap) %>%  setView(lng = -77.29, lat = 40.615, zoom = 7) %>% 
  #     addGeoJSON(paCounty, weight = 1, color = "#444444", fill = FALSE)
  # )
    
  # build download buttons
    output$downloadPAStateCaseData <- downloadMe(paRollingAvg, "PA Case Data")
    output$downloadPACountyCaseData <- downloadMe(casesCount %>% filter(county == input$casesCountyInput),
                                                  paste(input$casesCountyInput, "Data"))
    output$downloadAllPACountyCaseData <- downloadMe(casesCount, "All PA County Data")
    
  closeSweetAlert(session = session)
  sendSweetAlert(
    session = session,
    title ="Enjoy the data!",
    type = "success"
  )
    

  
}
