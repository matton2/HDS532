# server app for HDS532


server <- function(input, output, session) {
  
  progressSweetAlert(
    session = session, id = "apiProgress",
    title = "Getting data from the PA Website, Vaccine data first",
    display_pct = TRUE, value = 0
  )
  
  
  # for this application, I dont think we will need any reactive values but we will see
  
  countyVaccineDate <- RSocrata::read.socrata( "https://data.pa.gov/resource/bicw-3gwi.json",
                                     app_token = 'zqKFZgHzagrp74fam4vXZ7aWH') %>% 
    mutate(across(c(3:5), parse_integer))
  
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
    mutate(rolling7Day = round(roll_mean(cases, n = 7, align = "right", fill = NA),0)) %>% 
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
    mutate(rolling7Day = round(roll_mean(cases, n = 7, align = "right", fill = NA),0))
  
  
  updateProgressBar(
    session = session,
    id = "apiProgress",
    title = "Making Plots and Tables",
    value = 80
  )
  
  output$currentPACases <- renderValueBox(
    valueBox(
      paTotalsToday$todayCases,
      paste("Current Case Count for", paTotalsToday$date),
      icon = icon("biohazard")
    )
  )
  
  output$pa7DayRolling <- renderValueBox(
    valueBox(
      paRollingAvg$rolling7Day[NROW(paRollingAvg)],
      "Current 7 Day Rolling Average"
    )
  )
    
  closeSweetAlert(session = session)
  sendSweetAlert(
    session = session,
    title ="Enjoy the data!",
    type = "success"
  )
    

  
}
