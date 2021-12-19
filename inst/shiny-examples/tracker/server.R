library(shiny)
library(projectG1)
# to round the number to K, M,...
comprss <- function(tx) {
  div <- base::findInterval(base::as.numeric(base::gsub("\\,", "", tx)),
                            c(0, 1e3, 1e6, 1e9, 1e12) )  # modify this if negative numbers are possible
  base::paste(base::round( base::as.numeric(base::gsub("\\,","",tx))/10^(3*(div-1)), 2),
              c("","K","M","B","T")[div] )}

# Separator of thousand function
my_comma <- scales::label_comma(big.mark = "'", decimal.mark = ".")

#-------------------------------------------------------------------------------
# DATABASE
#-------------------------------------------------------------------------------

# Data collection
#----------------
database <- readr::read_csv(("https://covid.ourworldindata.org/data/owid-covid-data.csv"))

## Data cleaning
#---------------


database_EU <- database %>%
  dplyr::filter(continent == "Europe") %>% # filter European countries 140k obs -> 31k obs
  dplyr::filter(!location %in% c("Russia",
                                 "Gibraltar",
                                 "Isle of Man",
                                 "Jersey",
                                 "Guernsey",
                                 "Vatican")) %>% # remove some countries
  dplyr::select(c(date
                  ,iso_code
                  ,location
                  ,total_cases
                  ,total_cases_per_million
                  ,new_cases
                  ,new_cases_per_million
                  ,new_cases_smoothed
                  ,new_cases_smoothed_per_million
                  ,total_deaths
                  ,total_deaths_per_million
                  ,new_deaths
                  ,new_deaths_smoothed
                  ,new_deaths_smoothed_per_million
                  ,new_vaccinations
                  ,people_fully_vaccinated
                  ,people_fully_vaccinated_per_hundred
                  ,stringency_index
                  ,population
                  ,gdp_per_capita
                  ,median_age
                  ,cardiovasc_death_rate
                  ,diabetes_prevalence
                  ,human_development_index)) %>% # remove variables that have to many NAs
  dplyr::rename(country_code = iso_code)%>% # rename iso_code
  dplyr::group_by(location) %>% # group by location
  dplyr::arrange(date) %>% #arrange by date
  dplyr::mutate(new_cases_smoothed = abs(new_cases_smoothed)) %>% #revise all the values to positive
  tidyr::fill(c(people_fully_vaccinated_per_hundred,
                stringency_index,
                people_fully_vaccinated,
                total_cases)) %>% # Fill NA's with latest values
  dplyr::arrange(location) %>% #arrange by location
  dplyr::mutate(people_fully_vaccinated_per_hundred_MA =
                  zoo::rollmean(people_fully_vaccinated_per_hundred,
                                k=7,
                                fill = NA)) %>%  #smooth people_fully_vaccinated_per_hundred
  tidyr::fill(people_fully_vaccinated_per_hundred_MA) %>% # Fill NA's with latest values
  tibble::as_tibble() #transform to tibble


#-------------------------------------------------------------------------------
# SUB DATASETS
#-------------------------------------------------------------------------------

# Countries
#----------
countries <- unique(database_EU$location)

countries_fac <- as.factor(unique(database_EU$location))

# Dygraph
#--------
database_EU_dg <- database_EU %>%
  dplyr::select(date, new_cases_smoothed) %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(`EU Total Cases` = sum(new_cases_smoothed, na.rm = TRUE))


# Flags
#------
flags <- c(
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/al.svg", # Albania
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/ad.svg", # Andorra
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/at.svg", # Austria
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/by.svg", # Belarus
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/be.svg", # Belgium
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/ba.svg", # Bosnia & H
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/bg.svg", # Bulgaria
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/hr.svg", # Croatia
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/cy.svg", # Cyprus
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/cz.svg", # Czechia
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/dk.svg", # Denmark
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/ee.svg", # Estonia
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/fo.svg", # Feroe Islands
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/fi.svg", # Finland
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/fr.svg", # France
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/de.svg", # Germany
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/gr.svg", # Greece
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/hu.svg", # Hungary
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/is.svg", # Iceland
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/ie.svg", # Ireland
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/it.svg", # Italy
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/xk.svg", # Kosovo
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/lv.svg", # Latvia
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/li.svg", # Liechtenstein
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/lt.svg", # Lithuania
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/lu.svg", # Luxembourg
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/mt.svg", # Malta
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/md.svg", # Moldova
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/mc.svg", # Monaco
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/me.svg", # Montenegro
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/nl.svg", # Netherlands
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/mk.svg", # North Macedonia
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/no.svg", # Norway
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/pl.svg", # Poland
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/pt.svg", # Portugal
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/ro.svg", # Romania
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/sm.svg", # San Marino
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/rs.svg", # Serbia
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/sk.svg", # Slovakia
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/si.svg", # Slovenia
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/es.svg", # Spain
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/se.svg", # Sweden
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/ch.svg", # CH
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/ua.svg", # Ukraine
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/gb.svg" # UK
)

#-----
# MAP
#-----

# Color Settings
#-----------------
# colors for map color palette
pal_colors <- c("darkorange4",
                "peru",
                "tan",
                "wheat",
                "paleturquoise",
                "paleturquoise4",
                "darkslategrey")
# create the color palette
pal <- leaflet::colorNumeric(palette = pal_colors ,domain = c(0, 100))

# Coordinates
#------------
coordinates <- readr::read_csv(system.file("extdata","coordinates.csv",package = "projectG1")) %>% # open the coor. dataset
  dplyr::select(country_code = `Alpha-3 code`,
                latitude = `Latitude (average)`,
                longitude = `Longitude (average)`) %>% # select the var. of interest
  dplyr::filter(country_code != "NA") %>% # remove NAs
  dplyr::mutate_if(is.character, as.factor) # mutate characters to factors

# Dates
#------
available_dates <- database_EU %>%
  dplyr::arrange(date) %>% # arrange by date
  dplyr::distinct(date) # select distinct dates

# to have last date of dataset in other format
recent_date <- format(max(database_EU$date), format = "%B %d %Y")
recent_date_v <- format(max(database_EU$date - 7), format = "%B %d %Y")


shinyServer(function(input, output, session) {

  #-----------------------------------------------------------------------------
  # Page 1 : EU Map
  #-----------------------------------------------------------------------------

  # for display date under the title Global Summary
  formatted_date <- shiny::reactive({

    as.POSIXct(input$plot_date)

  })

  output$clean_date_reactive <- shiny::renderText({
    format(formatted_date(), "%d %B %Y")
  })

  # reactive_db to input$plot_date for Global summary
  reactive_db <- shiny::reactive({

    database_EU %>%
      dplyr::filter(date == formatted_date()) %>%
      dplyr::summarise(total_cases_EU = sum(total_cases),
                       total_deaths_EU = sum(total_deaths, na.rm = TRUE),
                       people_fully_vaccinated_EU = sum(people_fully_vaccinated, na.rm = TRUE))

  })

  # for display of total cases in Global Summary
  output$case_count <- shiny::renderText({

    paste0(prettyNum(reactive_db()$total_cases_EU, big.mark="'"), " cases")

  })

  # for display of people fully vacc. in Global Summary
  output$vacc_count <- shiny::renderText({

    paste0(prettyNum(reactive_db()$people_fully_vaccinated_EU, big.mark="'"), " vaccines")

  })

  # for display of total deaths in Global Summary
  output$death_count <- shiny::renderText({

    paste0(prettyNum(reactive_db()$total_deaths_EU, big.mark="'"), " deaths")

  })

  # EU COVID MAP
  output$map <- leaflet::renderLeaflet({

    # prepare the data for the map
    map_data <- database_EU %>%
      dplyr::group_by(location) %>% # group by location
      dplyr::filter(date == input$plot_date) %>% # react to input$plot_date
      dplyr::select(location
                    ,country_code
                    ,`new_cases`
                    ,`total_cases`
                    ,`new_deaths`
                    ,`total_deaths`
                    ,`people_fully_vaccinated_per_hundred`
                    ,`new_vaccinations`) %>% #select the var of interest
      #create the var for with the pop-up info
      dplyr::mutate(popup_info = paste(htmltools::strong(toupper(`location`)), "<br/>",
                                       "<B>New cases </B>", my_comma(`new_cases`), "<br/>",
                                       "<B>Total cases </B> ", my_comma(`total_cases`), "<br/>",
                                       "<B>New deaths</B> ", my_comma(`new_deaths`), "<br/>",
                                       "<B>Total deaths</B>", my_comma(`total_deaths`), "<br/>",
                                       "<B>New vaccinations</B>", my_comma(`new_vaccinations`), "<br/>",
                                       "<B>Population fully vacc.</B>", `people_fully_vaccinated_per_hundred`, "%")) %>%
      #add the coordinates to the dataset for the plot
      dplyr::left_join(coordinates, by = c("country_code"))

    # plot the map
    leaflet::leaflet(map_data) %>%
      leaflet::addProviderTiles(providers$CartoDB.Positron) %>% #map display
      leaflet::addCircles(radius = ~sqrt(total_cases)*70, #add circles and its settings
                          color = ~pal(`people_fully_vaccinated_per_hundred`),
                          stroke = T,
                          fillOpacity = 0.8,
                          opacity = 0.7,
                          popup = ~popup_info) %>%
      leaflet::addLegend("bottomright", #add the legend
                         pal = pal,
                         values = c(0,100),
                         na.label = "NA",
                         data = map_data$people_fully_vaccinated_per_hundred,
                         title = "Fully Vaccinated <br/> Population",
                         opacity = 0.7,
                         labFormat = leaflet::labelFormat(suffix = " %"))

  }) #renderLeaflet

  #-----------------------------------------------------------------------------
  # Page Data
  #-----------------------------------------------------------------------------

  # Display Dataset
  output$data <- DT::renderDT({
    database_EU %>%
      dplyr::relocate(people_fully_vaccinated_per_hundred_MA,
                      .after = people_fully_vaccinated_per_hundred) %>%
      DT::datatable(rownames = FALSE, # remove index column
                    filter = 'top', # add filter for all the variables
                    options = list(scrollX = TRUE, # to scroll horizontally from left to right
                                   autoWidth = TRUE,
                                   columnDefs = list(list(className = 'dt-center', targets = "_all")) # to center values and variables
                    ),
                    caption = paste('Last updated on', recent_date))
  })#renderDT

  #-----------------------------------------------------------------------------
  # Page General Trends
  #-----------------------------------------------------------------------------

  # DYGRAPH - SUM OF NEW CASES IN EUROPE
  output$dygraph <- dygraphs::renderDygraph({

    tsbox::ts_dygraphs(database_EU_dg,
                       height = 350,
                       width = 700,
                       main = "",
                       xlab = "Date",
                       ylab = "New Cases")  %>%
      dygraphs::dyRoller(rollPeriod = 1, showRoller = T) %>%
      dygraphs::dyRangeSelector(fillColor = grDevices::rgb(0.85,0.85,0.93,0.8)) %>%
      dygraphs::dyHighlight(highlightCircleSize = 5) %>%
      dygraphs::dySeries(color = "darkcyan") %>%
      dygraphs::dyOptions(fillGraph = TRUE # fill area
                          ,drawGrid = FALSE # remove grid
                          ,labelsKMB = "K")      # change y axis format
  }) #renderDygraph


  # REACTIVE DATASET for barplots
  data1 <- shiny::reactive({

    database_EU %>%
      dplyr::group_by(location) %>%
      dplyr::slice(which.max(as.Date(date, '%d/%m/%Y'))) # select row with most recent date by country (location)

  }) #reactive


  # BARPLOT - TOTAL CASES
  output$barplot_cases <- plotly::renderPlotly({

    plotly::ggplotly(
      ggplot2::ggplot(data1(),
                      aes(x = stats::reorder(location, -total_cases),  # order bar plot by total cases
                          y = total_cases,
                          text = paste('</br><b>Country:</b> ', location,
                                       '</br><b>Total cases:</b> ', comprss(total_cases),
                                       '</br><b>Date:</b> ', date))) +
        ggplot2::geom_bar(stat = "identity", fill = "dodgerblue4", alpha = 0.8) +
        ggplot2::scale_y_continuous(labels = scales::label_number_si()) +  # to format total cases numbers into Millions
        xlab("") +
        ylab("Total Cases") +
        ggplot2::ggtitle("") +
        ggplot2::theme_minimal()+
        ggplot2::theme(legend.position = "none",
                       panel.background = ggplot2::element_rect(fill = "#ffffff00",
                                                                color = "#ffffff00"), # make plot background transparent
                       plot.background = ggplot2::element_rect(fill = "#ffffff00",
                                                               color = "#ffffff00"),
                       axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5, hjust=1)), #to adjust label axis for better visibility),
      tooltip = "text")  #to only have text in the tooltip

  }) #renderPlotly


  # BARPLOT - TOTAL CASES PER MILLION
  output$barplot_cases_m <- plotly::renderPlotly({

    plotly::ggplotly(
      ggplot2::ggplot(data1(),
                      aes(x = stats::reorder(location, -total_cases_per_million), # order bar plot by total cases
                          y = total_cases_per_million,
                          text = paste('</br><b>Country:</b> ', location,
                                       '</br><b>Total cases per million:</b> ', scales::comma(total_cases_per_million),
                                       '</br><b>Date:</b> ', date))) +
        ggplot2::geom_bar(stat = "identity", fill = "dodgerblue4", alpha = 0.7) +
        ggplot2::scale_y_continuous(labels = scales::label_number_si()) +
        xlab("") +
        ylab("Total Cases per Million") +
        ggplot2::ggtitle("") +
        ggplot2::theme_minimal()+
        ggplot2::theme(legend.position = "none",
                       panel.background = ggplot2::element_rect(fill = "#ffffff00",
                                                                color = "#ffffff00"),   # make plot background transparent
                       plot.background = ggplot2::element_rect(fill = "#ffffff00",
                                                               color = "#ffffff00"),
                       axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5, hjust=1)),
      tooltip = "text")  #to only have text in the tooltip

  }) #renderPlotly


  # BARPLOT - TOTAL DEATHS
  output$barplot_deaths <- plotly::renderPlotly({

    plotly::ggplotly(
      ggplot2::ggplot(data1(),
                      aes(x = stats::reorder(location, -total_deaths), # order bar plot by total vacc
                          y = total_deaths,
                          text = paste('</br><b>Country:</b> ', location,
                                       '</br><b>Total Death:</b> ', scales::comma(total_deaths),
                                       '</br><b>Date:</b> ', date))) +
        ggplot2::geom_bar(stat = "identity", fill = "darkcyan") +
        ggplot2::scale_y_continuous(labels = scales::label_number_si()) +
        xlab("") +
        ylab("Total Deaths") +
        ggplot2::ggtitle("") +
        ggplot2::theme_minimal()+
        ggplot2::theme(legend.position = "none",
                       panel.background = ggplot2::element_rect(fill = "#ffffff00",
                                                                color = "#ffffff00"),   # make plot background transparent
                       plot.background = ggplot2::element_rect(fill = "#ffffff00",
                                                               color = "#ffffff00"),
                       axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5, hjust=1)),
      tooltip = "text")  #to only have text in the tooltip

  }) #renderPlotly


  # BARPLOT - TOTAL DEATHS PER MILLION
  output$barplot_deaths_m <- plotly::renderPlotly({

    plotly::ggplotly(
      ggplot2::ggplot(data1(),
                      aes(x = stats::reorder(location, -total_deaths_per_million), # order bar plot by total vacc
                          y = total_deaths_per_million,
                          text = paste('</br><b>Country:</b> ', location,
                                       '</br><b>Total Deaths per Million:</b> ', scales::comma(total_deaths_per_million),
                                       '</br><b>Date:</b> ', date))) +
        ggplot2::geom_bar(stat = "identity", fill = "darkcyan") +
        ggplot2::scale_y_continuous(labels = scales::label_number_si()) +
        xlab("") +
        ylab("Total Deaths per Million") +
        ggplot2::ggtitle("") +
        ggplot2::theme_minimal()+
        ggplot2::theme(legend.position = "none",
                       panel.background = ggplot2::element_rect(fill = "#ffffff00",
                                                                color = "#ffffff00"),   # make plot background transparent
                       plot.background = ggplot2::element_rect(fill = "#ffffff00",
                                                               color = "#ffffff00"),
                       axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5, hjust=1)),
      tooltip = "text")  #to only have text in the tooltip

  }) #renderPlotly


  # BARPLOT - VACCINES
  output$barplot_vaccines <- plotly::renderPlotly({

    plotly::ggplotly(
      ggplot2::ggplot(data1(),
                      aes(x = stats::reorder(location, -people_fully_vaccinated_per_hundred), # order bar plot by total vacc
                          y = people_fully_vaccinated_per_hundred,
                          text = paste('</br><b>Country:</b> ', location,
                                       '</br><b>Vaccination Rate:</b> ', paste( people_fully_vaccinated_per_hundred, "%"),
                                       '</br><b>Date:</b> ', date))) +
        ggplot2::geom_bar(stat = "identity", fill = "skyblue4") +
        xlab("") +
        ylab("Vaccination Rate (%)") +
        ggplot2::ggtitle("") +
        ggplot2::theme_minimal()+
        ggplot2::theme(legend.position = "none",
                       panel.background = ggplot2::element_rect(fill = "#ffffff00",
                                                                color = "#ffffff00"),   # make plot background transparent
                       plot.background = ggplot2::element_rect(fill = "#ffffff00",
                                                               color = "#ffffff00"),
                       axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5, hjust=1)),
      tooltip = "text")  #to only have text in the tooltip

  }) #renderPlotly

  # BUBBLE PLOT
  output$bubble <- plotly::renderPlotly({

    p1 <- ggplot2::ggplot(data1(),   # use filtered data on most recent date
                          aes(x = gdp_per_capita,
                              y = people_fully_vaccinated_per_hundred,
                              size = total_cases_per_million, #population
                              color = total_cases_per_million,
                              text = paste('</br><b>Country:</b> ', location,
                                           '</br><b>Total Cases per Million:</b> ', scales::comma(round(total_cases_per_million)),
                                           '</br><b>Vaccination Rate:</b> ', paste(people_fully_vaccinated_per_hundred,"%"),
                                           '</br><b>Population:</b> ', scales::comma(population),
                                           '</br><b>GDP per Capita:</b> ', paste(scales::comma(gdp_per_capita),"$")))) +
      ggplot2::geom_point(alpha = 0.8) +
      ggplot2::scale_size(range = c(.1, 10), name ="Population (M)")  +
      viridis::scale_color_viridis(alpha = 0.6,
                                   option="viridis",
                                   label = scales::comma) +
      ggplot2::scale_x_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
      ylab("Vaccination Rate (%)") +
      xlab("GDP per Capita ($)") +
      ggplot2::labs(color = "Total Cases per Million\n") +
      ggplot2::theme_minimal()+
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = "#ffffff00",
                                                              color = "#ffffff00"),   # make plot background transparent
                     plot.background = ggplot2::element_rect(fill = "#ffffff00",
                                                             color = "#ffffff00"))
    plotly::ggplotly(p1, tooltip = "text") %>%
      plotly::layout(xaxis = list(autorange = TRUE),
                     yaxis = list(autorange = TRUE))

  })  #renderPlotly

  #-----------------------------------------------------------------------------
  # Page Comparator
  #-----------------------------------------------------------------------------

  # FLAG SELECTION BOX
  output$txt <- shiny::renderText({

    paste(input$countries)

  }) #render text


  # REACTIVE DATA TO SELECTED COUNTRIES
  data2 <- shiny::reactive({

    shiny::req(input$country)
    database_EU %>%
      dplyr::filter(location %in% input$country) # filter the location selected by the user

  }) #reactive


  # COLOR PALETTE FOR THE FOLLOWING PLOTS
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


  # CASES PER DAY COMPARATOR
  output$comparator_1 <- plotly::renderPlotly({

    plotly::ggplotly(
      ggplot2::ggplot(data2(),
                      aes(x = date,
                          y = new_cases_smoothed,
                          color = location,
                          group = 1,
                          text = paste('</br><b>Country:</b> ', location,
                                       '</br><b>New cases (smoothed):</b> ', comprss(new_cases_smoothed),
                                       '</br><b>Date:</b> ', date))) +
        ggplot2::geom_line() +
        ggplot2::scale_colour_manual(values=cbPalette) +
        ggplot2::scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
        ggplot2::theme(legend.position = "none") +
        ggplot2::labs(x = "Date",
                      y = "New Cases") +
        ggplot2::theme_minimal()+
        ggplot2::theme(legend.position = "none",
                       panel.background = ggplot2::element_rect(fill = "#ffffff00",
                                                                color = "#ffffff00"),   # make plot background transparent
                       plot.background = ggplot2::element_rect(fill = "#ffffff00",
                                                               color = "#ffffff00"))
      ,tooltip = "text")

  }) #renderPlotly


  # CASES PER DAY PER MILLION COMPARATOR
  output$comparator_1bis <- plotly::renderPlotly({

    plotly::ggplotly(
      ggplot2::ggplot(data2(),
                      aes(x = date,
                          y = new_cases_smoothed_per_million,
                          color = location,
                          group = 1,
                          text = paste('</br><b>Country:</b> ', location,
                                       '</br><b>New cases per million (smoothed):</b> ', scales::comma(new_cases_smoothed_per_million,2),
                                       '</br><b>Date:</b> ', date))) +
        ggplot2::geom_line() +
        ggplot2::scale_colour_manual(values=cbPalette) +
        ggplot2::scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
        ggplot2::theme(legend.position = "none") +
        ggplot2::theme_minimal()+
        ggplot2::labs(x = "Date",
                      y = "New Cases per Million") +
        ggplot2::theme(legend.position = "none",
                       panel.background = ggplot2::element_rect(fill = "#ffffff00",
                                                                color = "#ffffff00"),   # make plot background transparent
                       plot.background = ggplot2::element_rect(fill = "#ffffff00",
                                                               color = "#ffffff00")),
      tooltip = "text")

  }) #renderPlotly


  # VACCINATION PER DAY COMPARATOR
  output$comparator_2 <- plotly::renderPlotly({

    plotly::ggplotly(
      ggplot2::ggplot(data2(),
                      aes(x = date,
                          y = people_fully_vaccinated_per_hundred_MA,
                          color = location,
                          group = 1,
                          text = paste('</br><b>Country:</b> ', location,
                                       '</br><b>Vaccination Rate:</b> ', paste(comprss(people_fully_vaccinated_per_hundred_MA), "%"),
                                       '</br><b>Date:</b> ', date))) +
        ggplot2::geom_line(na.rm = TRUE) +
        ggplot2::scale_colour_manual(values = cbPalette) +
        ggplot2::scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
        ggplot2::theme(legend.position = "none") +
        ggplot2::theme_minimal()+
        ggplot2::labs(x = "Date",
                      y = "Vaccination Rate (%)") +
        ggplot2::theme(legend.position = "none",
                       panel.background = ggplot2::element_rect(fill = "#ffffff00",
                                                                color = "#ffffff00"),   # make plot background transparent
                       plot.background = ggplot2::element_rect(fill = "#ffffff00",
                                                               color = "#ffffff00"))
      ,tooltip = "text")

  }) #renderPlotly

  # REACTIVE DATASET FOR BARPLOT COMPARATOR
  data3 <- shiny::reactive({

    shiny::req(input$country)

    database_EU %>%
      dplyr::group_by(location) %>%
      dplyr::slice(which.max(as.Date(date, '%d/%m/%Y'))) %>%
      dplyr::filter(location %in% input$country)

  }) #reactive

  # BARPLOT STRINGENCY INDEX
  output$comparator_barplot_si <- plotly::renderPlotly({

    p <-  data3() %>%
      ggplot2::ggplot(aes(x = stats::reorder(location, -stringency_index), # reorder barplots
                          y = stringency_index,
                          fill = input$country,
                          text = paste('</br><b>Country:</b> ', location,
                                       '</br><b>Stringency index:</b> ', stringency_index,
                                       '</br><b>Date:</b> ', date))) +
      ggplot2::geom_bar(stat = "identity", width = 0.5) +
      ggplot2::scale_fill_manual(values=cbPalette) +
      xlab("") +
      ylab("Current Stringency Index") +
      ggplot2::ggtitle("") +
      ggplot2::theme_minimal()+
      ggplot2::theme(legend.position = "none",
                     panel.background = ggplot2::element_rect(fill = "#ffffff00",
                                                              color = "#ffffff00"),   # make plot background transparent
                     plot.background = ggplot2::element_rect(fill = "#ffffff00",
                                                             color = "#ffffff00"))

    p %>% plotly::ggplotly(tooltip = "text")

  }) #renderPlotly


  # BARPLOT MEDIAN AGE
  output$comparator_barplot_median_age <- plotly::renderPlotly({

    p2 <-  data3() %>%
      ggplot2::ggplot(aes(x = stats::reorder(location, -median_age), # order bar plot by total vacc
                          y = median_age,
                          fill = input$country,
                          text = paste('</br><b>Country:</b> ', location,
                                       '</br><b>Median Age:</b> ', median_age,
                                       '</br><b>Date:</b> ', date))) +
      ggplot2::geom_bar(stat = "identity", width = 0.5) +
      ggplot2::scale_fill_manual(values=cbPalette) +
      xlab("") +
      ylab("Median Age") +
      ggplot2::ggtitle("") +
      ggplot2::theme_minimal()+
      ggplot2::theme(legend.position = "none",
                     panel.background = ggplot2::element_rect(fill = "#ffffff00",
                                                              color = "#ffffff00"),   # make plot background transparent
                     plot.background = ggplot2::element_rect(fill = "#ffffff00",
                                                             color = "#ffffff00"))

    p2 %>% plotly::ggplotly(tooltip = "text")


  }) #renderPlotly

  # BARPLOT DEATH
  output$comparator_barplot_death <- plotly::renderPlotly({

    p3 <-  data3() %>%
      ggplot2::ggplot(aes(x = stats::reorder(location, -new_deaths_smoothed), # order bar plot by total vacc
                          y = new_deaths_smoothed,
                          fill = input$country,
                          text = paste('</br><b>Country:</b> ', location,
                                       '</br><b>New deaths smoothed:</b> ', new_deaths_smoothed,
                                       '</br><b>Date:</b> ', date))) +
      ggplot2::geom_bar(stat = "identity", width = 0.5) +
      ggplot2::scale_fill_manual(values=cbPalette) +
      xlab("") +
      ylab("New Deaths Smoothed") +
      ggplot2::ggtitle("") +
      ggplot2::theme_minimal()+
      ggplot2::theme(legend.position = "none",
                     panel.background = ggplot2::element_rect(fill = "#ffffff00", color = "#ffffff00"),   # make plot background transparent
                     plot.background = ggplot2::element_rect(fill = "#ffffff00", color = "#ffffff00"))

    p3 %>% plotly::ggplotly(tooltip = "text")

  }) #renderPlotly


  # BARPLOT DEATHS PER MILLION
  output$comparator_barplot_death_pm <- plotly::renderPlotly({

    p4 <-  data3() %>%
      ggplot2::ggplot(aes(x = stats::reorder(location, -new_deaths_smoothed_per_million), # order bar plot by total vacc
                          y = new_deaths_smoothed_per_million,
                          fill = input$country,
                          text = paste('</br><b>Country:</b> ', location,
                                       '</br><b>New deaths smoothed per million:</b> ', new_deaths_smoothed_per_million,
                                       '</br><b>Date:</b> ', date))) +
      ggplot2::geom_bar(stat = "identity", width = 0.5) +
      ggplot2::scale_fill_manual(values=cbPalette) +
      # coord_flihtmltools::p() +
      # ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5, hjust=1))+
      xlab("") +
      ylab("New Deaths Smoothed per Million") +
      ggplot2::ggtitle("") +
      ggplot2::theme_minimal()+
      ggplot2::theme(legend.position = "none",
                     panel.background = ggplot2::element_rect(fill = "#ffffff00", color = "#ffffff00"),   # make plot background transparent
                     plot.background = ggplot2::element_rect(fill = "#ffffff00", color = "#ffffff00"))

    p4 %>% plotly::ggplotly(tooltip = "text")


  }) #renderPlotly


  # BARPLOT CARDIOVASC.
  output$comparator_barplot_cardio <- plotly::renderPlotly({

    p <-  data3() %>%
      ggplot2::ggplot(
        aes(x = stats::reorder(location, -cardiovasc_death_rate), # reorder barplots
            y = cardiovasc_death_rate,
            fill = input$country,
            text = paste('</br><b>Country:</b> ', location,
                         '</br><b>Cardiovascular Death Rate:</b> ', cardiovasc_death_rate,
                         '</br><b>Date:</b> ', date))) +
      ggplot2::geom_bar(stat = "identity", width = 0.5) +
      ggplot2::scale_fill_manual(values=cbPalette) +
      xlab("") +
      ylab("Cardiovascular Death Rate") +
      ggplot2::ggtitle("") +
      ggplot2::theme_minimal()+
      ggplot2::theme(legend.position = "none",
                     panel.background = ggplot2::element_rect(fill = "#ffffff00",
                                                              color = "#ffffff00"),   # make plot background transparent
                     plot.background = ggplot2::element_rect(fill = "#ffffff00",
                                                             color = "#ffffff00"))

    p %>% plotly::ggplotly(tooltip = "text")

  }) #renderPlotly

  # BARPLOT DIABETES
  output$comparator_barplot_diabetes <- plotly::renderPlotly({

    p <-  data3() %>%
      ggplot2::ggplot(aes(x = stats::reorder(location, -diabetes_prevalence), # reorder barplots
                          y = diabetes_prevalence,
                          fill = input$country,
                          text = paste('</br><b>Country:</b> ', location,
                                       '</br><b>Diabetes Prevalence:</b> ', diabetes_prevalence,
                                       '</br><b>Date:</b> ', date))) +
      ggplot2::geom_bar(stat = "identity", width = 0.5) +
      ggplot2::scale_fill_manual(values=cbPalette) +
      xlab("") +
      ylab("Diabetes Prevalence") +
      ggplot2::ggtitle("") +
      ggplot2::theme_minimal()+
      ggplot2::theme(legend.position = "none",
                     panel.background = ggplot2::element_rect(fill = "#ffffff00",
                                                              color = "#ffffff00"),   # make plot background transparent
                     plot.background = ggplot2::element_rect(fill = "#ffffff00",
                                                             color = "#ffffff00"))

    p %>% plotly::ggplotly(tooltip = "text")

  }) #renderPlotly


}) # SERVER END
