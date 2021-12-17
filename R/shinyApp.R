#' @title Eu covid19 tracker
#'
#' @author Aëllya & Laurène
#'
#' @return a shiny app for tracking covid19 in European countries
#'
#' @export
run_tracker <- function() {
  # load setup and database
  # Data collection
  database <- readr::read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

  ## Data cleaning

  # filter European countries from "continent"
  database_EU <- dplyr::as_tibble(dplyr::filter(database,continent == "Europe"))# 140k obs -> 31k observations remaining


  # remove variables that have to many NAs
  database_EU <-  dplyr::as_tibble(dplyr::filter(
    dplyr::rename(dplyr::select(dplyr::relocate(
      dplyr::filter(database_EU,location != "Russia"),
      c(date, continent),
      .before = iso_code),
      -c(weekly_icu_admissions
         ,weekly_icu_admissions_per_million
         ,handwashing_facilities
         ,weekly_hosp_admissions
         ,weekly_hosp_admissions_per_million
         ,total_boosters
         ,total_boosters_per_hundred
         ,excess_mortality_cumulative_absolute
         ,excess_mortality_cumulative
         ,excess_mortality
         ,excess_mortality_cumulative_per_million
         ,icu_patients
         ,icu_patients_per_million
         ,hosp_patients
         ,hosp_patients_per_million
         ,hospital_beds_per_thousand
         ,extreme_poverty
         ,tests_per_case
         ,tests_units)),
      country_code = iso_code),
    !is.na(total_cases)))



  # some negative absurd values in "new_cases", maybe replace them by 0
  #database_EU$new_cases[database_EU$new_cases < 0] <- 0


  # can be useful later on (map?)
  countries <- base::unique(database_EU$location)

  # dygraph
  database_EU_dg <- dplyr::select(database_EU,date, location, new_cases_smoothed) #-----------> TO DELETE IF WE DONT USE IT IN SHINY



  # MAP SUB-DATASETS
  #-------------------------------------------------------------------------------

  # DISPLAY SETTINGS
  #-----------------

  colors <- c("tan4", "turquoise4")
  pal <- leaflet::colorNumeric(palette = "BrBG" ,domain = c(0, 100))

  # COORDINATE DATA
  #----------------
  coordinates <- dplyr::mutate_if(dplyr::filter(dplyr::select(readr::read_csv(here::here("inst/map.data/coordinates.csv")),country_code = `Alpha-3 code`,
                                                              latitude = `Latitude (average)`,
                                                              longitude = `Longitude (average)`),country_code != "NA"),is.character, as.factor)

  # LAST DATE GLOBAL SUMMARY
  #-------------------------
  last_date <- dplyr::summarise(dplyr::group_by(dplyr::filter(database_EU,date == max(date)-7),date),total_cases_EU = sum(total_cases),
                                total_deaths_EU = sum(total_deaths, na.rm = TRUE),
                                people_fully_vaccinated_EU = sum(people_fully_vaccinated, na.rm = TRUE))




  source(here::here("R/setup.R"))



  ui <- navbarPage(title = "EU COVID-19 tracker",

                   # Page 1 : EU Map
                   #--------------------------------------------------------------

                   tabPanel("Map",
                            htmltools::h6("",
                               leafletOutput("map", height = "790px", width = "100%")),

                            #set the background opacity and color of the absolute panel
                            tags$style("
                                        #controls {
                                          background-color: azure3;
                                          opacity: 0.7;
                                        }
                                        #controls:hover{
                                          opacity: 0.8;
                                        }
                                               "),
                            #create the absolute panel
                            absolutePanel(id = "controls", class = "panel panel-default",
                                          top = 75, left = 55, width = 300, fixed=TRUE,
                                          draggable = TRUE, height = "auto",

                                          htmltools::h2("Global Summary", align = "center", style="color:darkcyan"),
                                          htmltools::h5(em(textOutput("last_date_title")), align = "center", style="color:darkcyan"),
                                          htmltools::h3(textOutput("vacc_count"), align = "right", style="color:saddlebrown"),
                                          htmltools::h4(textOutput("case_count"), align = "right", style="color:chocolate"),
                                          htmltools::h5(textOutput("death_count"), align = "right", style="color:sandybrown"),
                                          #plotOutput("epi_curve", height="130px", width="100%"),
                                          #plotOutput("cumulative_plot", height="130px", width="100%"),

                                          setSliderColor(c("darkcyan"), c(1)),
                                          sliderTextInput("plot_date",
                                                          label = htmltools::h6("Select mapping date", style="color:darkcyan"),
                                                          choices = unique(database_EU$date), #, "%d %b %y") #format(
                                                          to_fixed = last_date$date,
                                                          selected = last_date$date, #, "%d %b %y") #format(
                                                          grid = FALSE,
                                                          animate = animationOptions(interval = 3000, loop = FALSE))

                            )
                   ),





                   tabPanel("General Trend",

                            wellPanel(



                              fluidRow(column(9, # each row is made of 12 columns. Here we occupy 9 col for the dygraph
                                              htmltools::h4(strong("Evolution of New Cases in Europe")),
                                              htmltools::h5(em("Smoothed (7-day)")),
                                              dygraphOutput("plot1")),

                                       column(3, # occupy 3 col for the dygraph legend
                                              textOutput("legendDivID"))),

                              br(),
                              br(),
                              br(),
                              br(),

                              fluidRow(column(9, # put barplot in an other row to prevent plots overlaying
                                              h4(strong("Covid-19 Total Cases by European Countries")),
                                              plotlyOutput("plot2")))

                            )

                   ),


                   tabPanel("Comparator",
                            htmltools::h4(strong("Countries Comparator")),

                            #     plotlyOutput("")
                            selectizeInput(inputId = "country",   # create a drop down list
                                           label = "Select Country",
                                           choices = levels(as.factor(database_EU$location)),
                                           options = list(`actions-box` = TRUE),
                                           multiple = TRUE),

                            wellPanel(
                              plotlyOutput("plot3")
                            )




                   ),


                   ## Use navbarmenu to get the tab with menu capabilities
                   navbarMenu("Menu Options",
                              tabPanel("Menu item A - Summary stats", verbatimTextOutput("summary")),
                              tabPanel("Menu item B - Link to code",
                                       htmltools::h4(HTML(paste("We downloaded our DS here", a(href="https://github.com/owid/covid-19-data/raw/master/public/data", "link"), "."))),
                                       htmltools::h4(HTML(paste("In case you have questions", a(href="mailto:laurene.hsieh@gmail.com", "email me"), ".")))

                              )),

                   tabPanel("Data set",
                            wellPanel(
                              DT::dataTableOutput("data"))),

                   tabPanel("About Page",
                            htmltools::h4("This is a project for PTDS 2021 class")
                   )
  )



  server <- function(input, output, session) {

    # Page 1 : EU Map
    #-----------------------------------------------------------------------------

    # for display of the date under the title Global Summary
    output$last_date_title <- renderText({
      format(as.POSIXct(last_date$date),"%d %B %Y")
    })

    # for display of total cases in Global Summary
    output$case_count <- renderText({
      paste0(prettyNum(last_date$total_cases_EU, big.mark="'"), " cases")
    })

    # for display of total deaths in Global Summary
    output$death_count <- renderText({
      paste0(prettyNum(last_date$total_deaths_EU, big.mark="'"), " deaths")
    })

    # for display of people fully vacc. in Global Summary
    output$vacc_count <- renderText({
      paste0(prettyNum(last_date$people_fully_vaccinated_EU, big.mark="'"), " vaccines")
    })


    #EU COVID MAP
    output$map <- renderLeaflet({

      map_data <- database_EU %>% group_by(location) %>% #DATA FOR THE LEAFLET OUTPUT
        filter(date == input$plot_date) %>%
        select(location
               ,country_code
               ,`new_cases`
               ,`total_cases`
               ,`new_deaths`
               ,`total_deaths`
               ,`people_fully_vaccinated_per_hundred`
               ,`new_vaccinations`) %>%
        #inner_join(y, by = "location") %>%
        mutate(popup_info = paste(strong(toupper(`location`)), "<br/>",
                                  "<B> New case </B>", my_comma(`new_cases`), "<br/>",
                                  "<B>Total cases </B> ", my_comma(`total_cases`), "<br/>",
                                  "<B>New deaths</B> ", my_comma(`new_deaths`), "<br/>",
                                  "<B>Total Deaths</B>", my_comma(`total_deaths`), "<br/>",
                                  "<B>New vaccinations</B>", my_comma(`new_vaccinations`), "<br/>",
                                  "<B>Population fully vacc.</B>", `people_fully_vaccinated_per_hundred`, "%")) %>%
        left_join(coordinates, by = c("country_code"))

      leaflet(map_data) %>%
        addProviderTiles(providers$CartoDB.Positron) %>% #CartoDB.DarkMatterNoLabels
        addCircles(radius = ~sqrt(total_cases)*50,
                   color = ~pal(`people_fully_vaccinated_per_hundred`),
                   stroke = T,
                   fillOpacity = 0.8,
                   opacity = 0.7,
                   popup = ~popup_info) %>%
        leaflet::addLegend("bottomright",
                           pal = pal,
                           values = c(0,100),
                           na.label = "NA",
                           data = map_data$people_fully_vaccinated_per_hundred,
                           title = "Fully Vaccinated <br/> Population",
                           opacity = 0.7,
                           labFormat = labelFormat(suffix = " %"))

    })

    # Page Data
    #-----------------------------------------------------------------------------

    ### Display Dataset
    output$data <- renderDT({
      database_EU %>%
        datatable(rownames = FALSE,
                  options = list(scrollX = TRUE, # to scroll horizontally from left to right
                                 autoWidth = TRUE))
    })

    # Page General Trends
    #-----------------------------------------------------------------------------

    output$plot1 <- renderDygraph({

      ts_dygraphs(database_EU_dg, height = 350, width = 700, #from library tsbox
                  main = "",
                  xlab = "Date",
                  ylab = "New Cases")  %>%
        dyRoller(rollPeriod = 1, showRoller = T) %>%
        dyRangeSelector(fillColor = rgb(0.85,0.85,0.93,0.8)) %>%
        dyLegend(labelsDiv = "legendDivID") %>%
        dyOptions(fillGraph = TRUE, drawGrid = FALSE)


      #    database_EU_xts %>%
      #      dygraph(main = "European Covid-19 New Cases Smoothed (7-day) - Trend",
      #              xlab = "Date",
      #              ylab = "New Cases") %>%
      #      dyRoller(rollPeriod = 1, showRoller = T) %>%
      #      dyRangeSelector(fillColor = rgb(0.85,0.85,0.93,0.8)) %>%
      #      dyLegend(show = "always")

    })


    # for display of plots
    output$plot2 <- renderPlotly({

      data1 <- reactive({

        database_EU %>%
          group_by(location) %>%
          slice(which.max(as.Date(date, '%d/%m/%Y'))) # select row with most recent date by country (location)
      })

      ggplotly(
        ggplot(data1(),
               aes(x = reorder(location, -total_cases),  # order bar plot by total cases
                   y = total_cases,
                   text = paste('</br><b>Country:</b> ', location,
                                '</br><b>Total cases:</b> ', comprss(total_cases),
                                '</br><b>Date:</b> ', date
                   )
               )) +
          geom_bar(stat = "identity", fill = "steelblue") +
          theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) + #to adjust label axis for better visibility
          scale_y_continuous(labels = scales::label_number_si()) +  # to format total cases numbers into Millions
          xlab("") +
          ylab("Total cases") +
          ggtitle("") +
          theme(legend.position = "none",
                #          panel.grid = element_line(color = "#ffffff00"),
                #          axis.title = element_blank(),
                #          axis.text  = element_blank(),
                #          axis.ticks = element_blank(),
                panel.background = element_rect(fill = "#ffffff00", color = "#ffffff00"),   # make plot background transparent
                plot.background = element_rect(fill = "#ffffff00", color = "#ffffff00")),


        tooltip = "text")  #to only have text in the tooltip

    })

    # Page Comparator
    #-----------------------------------------------------------------------------

    # select country then plot

    data2 <- reactive({

      database_EU %>%
        filter(location %in% input$country)
    })

    # for display of plots
    output$plot3 <- renderPlotly({
      #   data <- database_EU %>% filter(location == input$country)


      ggplotly(
        ggplot(data2(), aes(x = date,
                            y = new_cases_smoothed,
                            colour = location)) +
          geom_line() +
          theme(legend.position = "none") +
          labs(x = "Date",
               y = "New Cases Smoothed")
      )
    })


    # Page "Menu item A page"
    #-----------------------------------------------------------------------------

    # for display of mtcars dataset summary statistics in the "Menu item A page"
    output$summary <- renderPrint({
      summary(mtcars)
    })

  }



  ################ APPLICATION ##################
  # make app appear
  shinyApp(ui = ui, server = server)

}

