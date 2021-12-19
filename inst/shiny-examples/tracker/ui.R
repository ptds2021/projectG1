

  #-------------------------------------------------------------------------------
  # DATABASE
  #-------------------------------------------------------------------------------
  library(tidyverse)
  library(tidyr)
  library(shiny)
  #source(here::here("inst/shiny-examples/tracker/setup.R"))
  library(shiny)
  devtools::load_all()



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


  shinyUI(shiny::navbarPage(theme = shinythemes::shinytheme("yeti"),

                            #--------------------------------------------------------------
                            # Title
                            #--------------------------------------------------------------

                            title = "EU COVID-19 tracker",

                            #--------------------------------------------------------------
                            # Page 1 : EU Map
                            #--------------------------------------------------------------

                            shiny::tabPanel("Map", # tab title
                                            htmltools::div(class="outer",
                                                           tags$head(
                                                             # Include our custom CSS
                                                             htmltools::includeCSS("styles.css")
                                                           ),
                                                           # Map
                                                           leaflet::leafletOutput("map",
                                                                                  height = "100%",
                                                                                  width = "100%"),

                                                           #set the background opacity and color of the absolute panel
                                                           tags$style("
                                        #controls {
                                          background-color: azure3;
                                          opacity: 0.7;
                                        }
                                        #controls:hover{
                                          opacity: 0.7;
                                        }
                                               "),

                                                           #create the absolute panel
                                                           shiny::absolutePanel(
                                                             # Absolute Panel settings
                                                             id = "controls",
                                                             class = "panel panel-default",
                                                             top = 80,
                                                             left = 55,
                                                             width = 300,
                                                             fixed=TRUE,
                                                             draggable = TRUE,
                                                             height = "auto",
                                                             # Absolute Panel content
                                                             htmltools::h2("Global Summary", align = "left", style="color:gray29"),
                                                             htmltools::h5(htmltools::em(shiny::textOutput("clean_date_reactive")), align = "center"),
                                                             htmltools::h3(shiny::textOutput("case_count"), align = "right", style="color:saddlebrown"),
                                                             htmltools::h4(shiny::textOutput("vacc_count"), align = "right", style="color:chocolate"),
                                                             htmltools::h5(shiny::textOutput("death_count"), align = "right", style="color:#ef7a17"),
                                                             shinyWidgets::setSliderColor(c("darkcyan"), c(1)), #slider color
                                                             shinyWidgets::sliderTextInput("plot_date",
                                                                                           # Slider options
                                                                                           label = htmltools::h6("Select mapping date", style="color:darkcyan"),
                                                                                           choices = available_dates$date, #, "%d %b %y") #format(
                                                                                           selected = max(available_dates$date), #, "%d %b %y") #format(
                                                                                           grid = FALSE,
                                                                                           animate = shiny::animationOptions(interval = 2000, loop = FALSE))
                                                           )#absolutePanel
                                            )#div
                            ),#tabPanel Map


                            #--------------------------------------------------------------
                            # Page 2 : Visualizations
                            #--------------------------------------------------------------
                            shiny::navbarMenu("Data Visualizations",

                                              #-------------------------------------------------------
                                              # General Trend
                                              #-------------------------------------------------------

                                              shiny::tabPanel("General Trend",
                                                              htmltools::h3(htmltools::strong("European COVID-19 General Trend")),
                                                              shiny::wellPanel(


                                                                # NEW CASES PLOT

                                                                shiny::fluidRow(shiny::column(9, # each row is made of 12 columns. Here we occupy 9 col for the dygraph
                                                                                              htmltools::h4(htmltools::strong("Evolution of New Cases in Europe")),
                                                                                              htmltools::h5(htmltools::em("7-day rolling average")),
                                                                                              dygraphs::dygraphOutput("dygraph") %>%
                                                                                                shinycssloaders::withSpinner(color="#0dc5c1")), # add busy icon while downloading
                                                                                shiny::column(3, # occupy 3 col for the dygraph legend
                                                                                              shiny::br(), #add some space between plots
                                                                                              shiny::br(),
                                                                                              shiny::br(),
                                                                                              shiny::br(),
                                                                                              htmltools::p(htmltools::strong("Note:")),
                                                                                              htmltools::p("New COVID-19 cases per day were smoothed with a 7-day rolling average.
                                                          This means that the values for each day were computed by averaging the
                                                          values of this day and the six days before.",
                                                                                                           style = "font-family: 'arial'; font-si18pt"))), # to have legend of dygraph on the right
                                                                shiny::br(), #add some space between plots
                                                                shiny::br(),
                                                                shiny::br(),
                                                                shiny::br(),


                                                                # TOTAL CASES PLOT

                                                                shiny::fluidRow(shiny::column(9, # put barplot in an other row to prevent plots overlaying
                                                                                              htmltools::h4(htmltools::strong("Total Cases by European Countries")),
                                                                                              htmltools::h5(htmltools::em(paste("On", recent_date))),
                                                                                              shiny::tabsetPanel(type = "tabs", #Put plots in tabs
                                                                                                                 shiny::tabPanel("Total Cases",
                                                                                                                                 plotly::plotlyOutput("barplot_cases")),
                                                                                                                 shiny::tabPanel("Total Cases per Million",
                                                                                                                                 plotly::plotlyOutput("barplot_cases_m"))
                                                                                              )),#tabsetPanel
                                                                                shiny::br(), #add some space between plots and note
                                                                                shiny::br(),
                                                                                shiny::br(),
                                                                                shiny::br(),
                                                                                shiny::br(),
                                                                                shiny::column(3, # occupy 3 col for the dygraph legend
                                                                                              htmltools::p(htmltools::strong("Note:")),
                                                                                              htmltools::p("Viewing the data per million people will reorder
                                                          the countries relative to one another. Indeed, the
                                                          biggest countries will no longer be the most affected
                                                          after adjusting the size of the populations in the countries",
                                                                                                           style = "font-family: 'arial'; font-si18pt"))),


                                                                # TOTAL DEATHS PLOT

                                                                shiny::fluidRow(shiny::column(9,
                                                                                              htmltools::h4(htmltools::strong("Total Deaths by European Countries")),
                                                                                              htmltools::h5(htmltools::em(paste("On", recent_date))),
                                                                                              shiny::tabsetPanel(type = "tabs",
                                                                                                                 shiny::tabPanel("Total Deaths",
                                                                                                                                 plotly::plotlyOutput("barplot_deaths")),
                                                                                                                 shiny::tabPanel("Total Deaths per Million",
                                                                                                                                 plotly::plotlyOutput("barplot_deaths_m"))
                                                                                              )#tabsetPanel
                                                                )),#column & fluidRow

                                                                # VACCINATION PLOT

                                                                shiny::fluidRow(shiny::column(9,
                                                                                              htmltools::h4(htmltools::strong("Vaccination Rate by European Countries")),
                                                                                              htmltools::h5(htmltools::em(paste("On", recent_date))),
                                                                                              plotly::plotlyOutput("barplot_vaccines")
                                                                )),#column & fluidRow

                                                                # BUBBLE PLOT

                                                                shiny::fluidRow(shiny::column(9,
                                                                                              htmltools::h4(htmltools::strong("Bubble Chart and COVID-19 Total Cases per Million")),
                                                                                              htmltools::h5(htmltools::em(paste("On", recent_date))),  # to display most recent_date
                                                                                              plotly::plotlyOutput("bubble")),
                                                                                shiny::column(3, # occupy 3 col for the dygraph legend
                                                                                              shiny::br(), #add some space between plots
                                                                                              shiny::br(),
                                                                                              shiny::br(),
                                                                                              shiny::br(),
                                                                                              shiny::br(),
                                                                                              shiny::br(),
                                                                                              shiny::br(),
                                                                                              shiny::br(),
                                                                                              shiny::br(),
                                                                                              htmltools::p(htmltools::strong("Note:")),
                                                                                              htmltools::p("The circles diameter, such as the color, is proportional to the Total
                                                          Cases Per Millions in the different countries.",
                                                                                                           style = "font-family: 'arial'; font-si18pt")
                                                                                ))#column & fluidRow
                                                              )), #wellPanel & tabPanel


                                              #-------------------------------------------------------
                                              #  Countries Comparator
                                              #-------------------------------------------------------

                                              shiny::tabPanel("Comparator",
                                                              htmltools::h3(htmltools::strong("Countries Comparator")),
                                                              shiny::wellPanel(


                                                                # COUNTRIES PICKER

                                                                htmltools::h5(htmltools::strong("Select up to 5 countries")),
                                                                shinyWidgets::pickerInput(inputId = "country",   # create a drop down list
                                                                                          selected = "Switzerland",
                                                                                          choices = levels(as.factor(database_EU$location)),
                                                                                          multiple = TRUE,
                                                                                          width = "fit",
                                                                                          choicesOpt = list(content =
                                                                                                              mapply(countries,
                                                                                                                     flags,
                                                                                                                     FUN = function(country, flagUrl) {
                                                                                                                       htmltools::HTML(paste(
                                                                                                                         tags$img(src=flagUrl,
                                                                                                                                  width=20,
                                                                                                                                  height=15),
                                                                                                                         country))
                                                                                                                     }, SIMPLIFY = FALSE, USE.NAMES = FALSE)),
                                                                                          options = list(
                                                                                            size = 5,
                                                                                            "live-search" = TRUE,
                                                                                            "max-options" = 5,
                                                                                            "max-options-text" = "It says select up to FIVE countries!")
                                                                ),#pickerInput


                                                                # UP TO 5 COUNTRIES RECOMMENDATION

                                                                shiny::textOutput("txt"),


                                                                # NEW CASES PLOT

                                                                shiny::fluidRow(shiny::column(9, # each row is made of 12 columns. Here we occupy 9 col for the dygraph
                                                                                              htmltools::h4(htmltools::strong("New COVID-19 Cases by Day")),
                                                                                              htmltools::h5(htmltools::em("7-day rolling average")),
                                                                                              shiny::tabsetPanel(type = "tabs",
                                                                                                                 shiny::tabPanel("New Cases",
                                                                                                                                 plotly::plotlyOutput("comparator_1") %>%
                                                                                                                                   shinycssloaders::withSpinner(color="#0dc5c1")),
                                                                                                                 shiny::tabPanel("New Cases per Million",
                                                                                                                                 plotly::plotlyOutput("comparator_1bis") %>%
                                                                                                                                   shinycssloaders::withSpinner(color="#0dc5c1")))
                                                                ),#column
                                                                shiny::column(3, # occupy 3 col for the dygraph legend
                                                                              shiny::br(), #add some space between plots
                                                                              shiny::br(),
                                                                              shiny::br(),
                                                                              shiny::br(),
                                                                              shiny::br(),
                                                                              htmltools::p(htmltools::strong("Note:")),
                                                                              htmltools::p("New COVID-19 Cases by Day and Vaccination Rate by Day were smoothed with
                                                          a 7-day rolling average.This means that the values for each day were
                                                          computed by averaging the values of this day and the six days before.",
                                                                                           style = "font-family: 'arial'; font-si18pt"))),


                                                                # VACCINATION PLOT

                                                                shiny::fluidRow(shiny::column(9, # each row is made of 12 columns. Here we occupy 9 col for the dygraph
                                                                                              htmltools::h4(htmltools::strong("Vaccination Rate by Day")),
                                                                                              htmltools::h5(htmltools::em("7-day rolling average")),
                                                                                              plotly::plotlyOutput("comparator_2") %>%
                                                                                                shinycssloaders::withSpinner(color="#0dc5c1"))),

                                                                # DEATHS & DEMOGRAPHIC PLOTS
                                                                htmltools::h4(htmltools::strong("Deaths and Demographic Features")),
                                                                shiny::fluidRow(shiny::column(6,
                                                                                              # New deaths plots in panel
                                                                                              shiny::tabsetPanel(type = "tabs",
                                                                                                                 shiny::tabPanel("New Deaths",
                                                                                                                                 plotly::plotlyOutput("comparator_barplot_death")),
                                                                                                                 shiny::tabPanel("New Deaths per Million",
                                                                                                                                 plotly::plotlyOutput("comparator_barplot_death_pm"))
                                                                                              ) #tabsetpanel
                                                                ),#column
                                                                shiny::column(6,
                                                                              # Demographic plots in panel
                                                                              shiny::tabsetPanel(type = "tabs",
                                                                                                 shiny::tabPanel("Stringency",
                                                                                                                 plotly::plotlyOutput("comparator_barplot_si") %>%
                                                                                                                   shinycssloaders::withSpinner(color="#0dc5c1"),
                                                                                                                 htmltools::h6(htmltools::em("Stringency Index:")),
                                                                                                                 htmltools::h6(htmltools::em("A composite measure based on 9
                                                                                                      response indicators including
                                                                                                      school closures, workplace
                                                                                                      closures, and travel bans,
                                                                                                      rescaled to a value from 0 to
                                                                                                     100 [from flexible to strict]."))),
                                                                                                 shiny::tabPanel("Median Age",
                                                                                                                 plotly::plotlyOutput("comparator_barplot_median_age") %>%
                                                                                                                   shinycssloaders::withSpinner(color="#0dc5c1"),
                                                                                                                 htmltools::h6(htmltools::em("Median Age:")),
                                                                                                                 htmltools::h6(htmltools::em("United Nations projection for
                                                                                                     2020 median age of the
                                                                                                     population"))),
                                                                                                 shiny::tabPanel("Cardiovasc. Deaths",
                                                                                                                 plotly::plotlyOutput("comparator_barplot_cardio") %>%
                                                                                                                   shinycssloaders::withSpinner(color="#0dc5c1"),
                                                                                                                 htmltools::h6(htmltools::em("Cardiovascular Death Rate:")),
                                                                                                                 htmltools::h6(htmltools::em("Death rate from cardiovascular
                                                                                                     disease in 2017 (annual number
                                                                                                     of deaths per 100'000 people)"))),
                                                                                                 shiny::tabPanel("Diabete Prev.",
                                                                                                                 plotly::plotlyOutput("comparator_barplot_diabetes") %>%
                                                                                                                   shinycssloaders::withSpinner(color="#0dc5c1"),
                                                                                                                 htmltools::h6(htmltools::em("Diabetes Prevalence:")),
                                                                                                                 htmltools::h6(htmltools::em("Diabetes prevalence (% of
                                                                                                     population aged 20 to 79) in 2017")))
                                                                              ) #tabsetpanel
                                                                )#column
                                                                )   #fluidRow
                                                              )#wellPanel
                                              )), #tabPanel & NavBarMenu


                            #--------------------------------------------------------------
                            # Page 3 : Data Set
                            #--------------------------------------------------------------

                            shiny::tabPanel("Data set",
                                            shiny::titlePanel(htmltools::strong("Data")),
                                            shiny::wellPanel(
                                              DT::dataTableOutput("data") %>%
                                                shinycssloaders::withSpinner(color="#0dc5c1"))
                            ), #tabPanel

                            #--------------------------------------------------------------
                            # Page 4 : About
                            #--------------------------------------------------------------

                            shiny::tabPanel("About",
                                            tags$style(".topimg {
                                        margin-left:-30px;
                                        margin-right:-15px;
                                        margin-top:-25px;
                                       }"),
                                            tags$style(".logo {
                                        position:fixed;
                                        left:10px;
                                        bottom: 10px
                                       }"),
                                            htmltools::div(class="topimg",htmltools::img(src="assets/about3.png", # add top image
                                                                                         height="100%",
                                                                                         width="100%")),
                                            shiny::titlePanel(htmltools::strong("About This App")),
                                            shiny::sidebarLayout(
                                              shiny::sidebarPanel(
                                                width = 3,
                                                htmltools::h3("Why?"),
                                                htmltools::h5(htmltools::em("The COVID-19 pandemic has been striking since the
                                                 beginning of 2020 and has heavily affected all areas
                                                 of social and economic activity around the world. The
                                                 crisis is not over and is currently reshaping our
                                                 society. That is why we decided to focus on this topic
                                                 for our Programming Tools in Data Science class project."))
                                              ),#sidebarPanel
                                              shiny::mainPanel(
                                                htmltools::h3("Goals and Impacts"),
                                                htmltools::h5("Our main goal was to create a useful online interactive dashboard that
                                              visualizes and tracks confirmed cases of COVID-19 in real-time across Europe
                                              over time with the impact of vaccination campaigns in the different countries."),
                                                htmltools::h5("This dashboard is intended as a user-friendly dashboard for researchers as well
                                              as the general public to track the COVID-19 pandemic and evolution since the
                                              vaccinations began. It is generated from trusted data sources and built in
                                              open-source R software (Shiny) which serves as a platform for visualization and
                                              analysis of the data providing real-time statistical application aiming to make sense
                                              to academic and public consumers of the large amount of data that is being accumulated
                                              due to the COVID-19 pandemic."),
                                                htmltools::h5("Our aim is for users to have a set of relevant pages displaying key COVID-19 statistics
                                              interactively, like the ability to change the country, date, and statistics they wish
                                              to view. The goal of this project is to also showcase the strengths of data science to
                                              tackle one of the worlds most difficult problems: predict and track the effect of
                                              a pandemic on daily lives. Being designed as a cross-platform web-browser accessible
                                              dashboard (portal) to display massive data, the R Shiny environment provides a platform
                                              for end-users to interact and visualize the data according to their needs."),
                                                htmltools::h3("Data Source"),
                                                htmltools::h5(htmltools::HTML(paste("Our data comes from",
                                                                                    htmltools::a(href="https://github.com/owid/covid-19-data/raw/master/public/data",
                                                                                                 "Our World in Data GitHub."),
                                                                                    "The data is updated daily."))),
                                                htmltools::h3("Codes"),
                                                htmltools::h5(htmltools::HTML(paste("Code and input data used to generate this Shiny mapping tool are available on",
                                                                                    htmltools::a(href="https://github.com/ptds2021/projectG1","Github"),"."))),
                                                htmltools::h3("Authors"),
                                                tags$li(htmltools::HTML(paste("Laurene Hsieh, MSc in Management and Business Analytics,",
                                                                              htmltools::a(href="mailto:laurene.hsieh@unil.ch", "laurene.hsieh@unil.ch")))),
                                                tags$li(htmltools::HTML(paste("Visesa Jain, MSc in Management,",
                                                                              htmltools::a(href="mailto:visesa.jain@unil.ch", "visesa.jain@unil.ch")))),
                                                tags$li(htmltools::HTML(paste("Aellya Monney, MSc in Management and Business Analytics,",
                                                                              htmltools::a(href="mailto:aellya.monney@unil.ch", "aellya.monney@unil.ch")))),
                                                tags$li(htmltools::HTML(paste("Corinne Schonholzer, MSc in Accounting, Control and Finance,",
                                                                              htmltools::a(href="mailto:corinne.schonholzer@unil.ch", "corinne.schonholzer@unil.ch")))),
                                                tags$li(htmltools::HTML(paste("Remy Tombola, MSc in Management and Business Analytics,",
                                                                              htmltools::a(href="mailto:remy.tombola@unil.ch", "remy.tombola@unil.ch")))),
                                                shiny::br(),
                                                shiny::br(),
                                                htmltools::div(class=".logo",htmltools::img(src="assets/logohec.png",    # HEC logo
                                                                                            height="20%",
                                                                                            width="20%"))
                                              ) #sidebarPanel
                                            ) #sidebarLayout
                            ) #tabPanel


  )) #navbarPage END OF UI
