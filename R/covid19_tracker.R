# load setup and database

#' @title Eu covid19 tracker
#'
#' @author Aëllya & Laurène
#'
#' @return a shiny app for tracking covid19 in European countries
#'
#' @export
run_tracker <- function() {

  devtools::load_all()
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
                                   "Guernsey")) %>% # remove some countries
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
    "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/gb.svg", # UK
    "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/va.svg" # Vatican
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
  coordinates <- readr::read_csv(here::here("inst/coordinates.csv")) %>% # open the coor. dataset
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

  ui <- shiny::navbarPage(theme = shinytheme("yeti"),

                          #--------------------------------------------------------------
                          # Title
                          #--------------------------------------------------------------

                          title = "EU COVID-19 tracker",

                          #--------------------------------------------------------------
                          # Page 1 : EU Map
                          #--------------------------------------------------------------

                          shiny::tabPanel("Map", # tab title
                                          div(class="outer",
                                              tags$head(
                                                # Include our custom CSS
                                                includeCSS("inst/styles.css")
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
                                                htmltools::h5(em(shiny::textOutput("clean_date_reactive")), align = "center"),
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
                                                                              animate = animationOptions(interval = 2000, loop = FALSE))
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
                                                            htmltools::h3(strong("European COVID-19 General Trend")),
                                                            shiny::wellPanel(


                                                              # NEW CASES PLOT

                                                              shiny::fluidRow(shiny::column(9, # each row is made of 12 columns. Here we occupy 9 col for the dygraph
                                                                                            htmltools::h4(strong("Evolution of New Cases in Europe")),
                                                                                            htmltools::h5(em("7-day rolling average")),
                                                                                            dygraphs::dygraphOutput("dygraph") %>%
                                                                                              shinycssloaders::withSpinner(color="#0dc5c1")), # add busy icon while downloading
                                                                              shiny::column(3, # occupy 3 col for the dygraph legend
                                                                                            shiny::br(), #add some space between plots
                                                                                            shiny::br(),
                                                                                            shiny::br(),
                                                                                            shiny::br(),
                                                                                            p(strong("Note:")),
                                                                                            p("New COVID-19 cases per day were smoothed with a 7-day rolling average.
                                                          This means that the values for each day were computed by averaging the
                                                          values of this day and the six days before.",
                                                                                              style = "font-family: 'arial'; font-si18pt"))), # to have legend of dygraph on the right
                                                              shiny::br(), #add some space between plots
                                                              shiny::br(),
                                                              shiny::br(),
                                                              shiny::br(),


                                                              # TOTAL CASES PLOT

                                                              shiny::fluidRow(shiny::column(9, # put barplot in an other row to prevent plots overlaying
                                                                                            htmltools::h4(strong("Total Cases by European Countries")),
                                                                                            htmltools::h5(em(paste("On", recent_date))),
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
                                                                                            p(strong("Note:")),
                                                                                            p("Viewing the data per million people will reorder
                                                          the countries relative to one another. Indeed, the
                                                          biggest countries will no longer be the most affected
                                                          after adjusting the size of the populations in the countries",
                                                                                              style = "font-family: 'arial'; font-si18pt"))),


                                                              # TOTAL DEATHS PLOT

                                                              shiny::fluidRow(shiny::column(9,
                                                                                            htmltools::h4(strong("Total Deaths by European Countries")),
                                                                                            htmltools::h5(em(paste("On", recent_date))),
                                                                                            shiny::tabsetPanel(type = "tabs",
                                                                                                               shiny::tabPanel("Total Deaths",
                                                                                                                               plotly::plotlyOutput("barplot_deaths")),
                                                                                                               shiny::tabPanel("Total Deaths per Million",
                                                                                                                               plotly::plotlyOutput("barplot_deaths_m"))
                                                                                            )#tabsetPanel
                                                              )),#column & fluidRow

                                                              # VACCINATION PLOT

                                                              shiny::fluidRow(shiny::column(9,
                                                                                            htmltools::h4(strong("Vaccination Rate by European Countries")),
                                                                                            htmltools::h5(em(paste("On", recent_date))),
                                                                                            plotly::plotlyOutput("barplot_vaccines")
                                                              )),#column & fluidRow

                                                              # BUBBLE PLOT

                                                              shiny::fluidRow(shiny::column(9,
                                                                                            htmltools::h4(strong("Bubble Chart and COVID-19 Total Cases per Million")),
                                                                                            htmltools::h5(em(paste("On", recent_date))),  # to display most recent_date
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
                                                                                            p(strong("Note:")),
                                                                                            p("The circles diameter, such as the color, is proportional to the Total
                                                          Cases Per Millions in the different countries.",
                                                                                              style = "font-family: 'arial'; font-si18pt")
                                                                              ))#column & fluidRow
                                                            )), #wellPanel & tabPanel


                                            #-------------------------------------------------------
                                            #  Countries Comparator
                                            #-------------------------------------------------------

                                            shiny::tabPanel("Comparator",
                                                            htmltools::h3(strong("Countries Comparator")),
                                                            shiny::wellPanel(


                                                              # COUNTRIES PICKER

                                                              htmltools::h5(strong("Select up to 5 countries")),
                                                              shinyWidgets::pickerInput(inputId = "country",   # create a drop down list
                                                                                        selected = "Switzerland",
                                                                                        choices = levels(as.factor(database_EU$location)),
                                                                                        multiple = TRUE,
                                                                                        width = "fit",
                                                                                        choicesOpt = list(content =
                                                                                                            mapply(countries,
                                                                                                                   flags,
                                                                                                                   FUN = function(country, flagUrl) {
                                                                                                                     HTML(paste(
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
                                                                                            htmltools::h4(strong("New COVID-19 Cases by Day")),
                                                                                            htmltools::h5(em("7-day rolling average")),
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
                                                                            p(strong("Note:")),
                                                                            p("New COVID-19 Cases by Day and Vaccination Rate by Day were smoothed with
                                                          a 7-day rolling average.This means that the values for each day were
                                                          computed by averaging the values of this day and the six days before.",
                                                                              style = "font-family: 'arial'; font-si18pt"))),


                                                              # VACCINATION PLOT

                                                              shiny::fluidRow(shiny::column(9, # each row is made of 12 columns. Here we occupy 9 col for the dygraph
                                                                                            htmltools::h4(strong("Vaccination Rate by Day")),
                                                                                            htmltools::h5(em("7-day rolling average")),
                                                                                            plotly::plotlyOutput("comparator_2") %>%
                                                                                              shinycssloaders::withSpinner(color="#0dc5c1"))),

                                                              # DEATHS & DEMOGRAPHIC PLOTS
                                                              htmltools::h4(strong("Deaths and Demographic Features")),
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
                                                                                                               htmltools::h6(em("Stringency Index:")),
                                                                                                               htmltools::h6(em("A composite measure based on 9
                                                                                                      response indicators including
                                                                                                      school closures, workplace
                                                                                                      closures, and travel bans,
                                                                                                      rescaled to a value from 0 to
                                                                                                     100 [from flexible to strict]."))),
                                                                                               shiny::tabPanel("Median Age",
                                                                                                               plotly::plotlyOutput("comparator_barplot_median_age") %>%
                                                                                                                 shinycssloaders::withSpinner(color="#0dc5c1"),
                                                                                                               htmltools::h6(em("Median Age:")),
                                                                                                               htmltools::h6(em("United Nations projection for
                                                                                                     2020 median age of the
                                                                                                     population"))),
                                                                                               shiny::tabPanel("Cardiovasc. Deaths",
                                                                                                               plotly::plotlyOutput("comparator_barplot_cardio") %>%
                                                                                                                 shinycssloaders::withSpinner(color="#0dc5c1"),
                                                                                                               htmltools::h6(em("Cardiovascular Death Rate:")),
                                                                                                               htmltools::h6(em("Death rate from cardiovascular
                                                                                                     disease in 2017 (annual number
                                                                                                     of deaths per 100'000 people)"))),
                                                                                               shiny::tabPanel("Diabete Prev.",
                                                                                                               plotly::plotlyOutput("comparator_barplot_diabetes") %>%
                                                                                                                 shinycssloaders::withSpinner(color="#0dc5c1"),
                                                                                                               htmltools::h6(em("Diabetes Prevalence:")),
                                                                                                               htmltools::h6(em("Diabetes prevalence (% of
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
                                          shiny::titlePanel(strong("Data")),
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
                                          div(class="topimg",img(src="assets/about3.png", # add top image
                                                                 height="100%",
                                                                 width="100%")),
                                          shiny::titlePanel(strong("About This App")),
                                          shiny::sidebarLayout(
                                            shiny::sidebarPanel(
                                              width = 3,
                                              htmltools::h3("Why?"),
                                              htmltools::h5(em("The COVID-19 pandemic has been striking since the
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
                                              htmltools::h5(HTML(paste("Our data comes from",
                                                                       a(href="https://github.com/owid/covid-19-data/raw/master/public/data",
                                                                         "Our World in Data GitHub."),
                                                                       "The data is updated daily."))),
                                              htmltools::h3("Codes"),
                                              htmltools::h5(HTML(paste("Code and input data used to generate this Shiny mapping tool are available",
                                                                       a(href="https://github.com/ptds2021/projectG1 on","Github"),"."))),
                                              htmltools::h3("Authors"),
                                              tags$li(HTML(paste("Laurene Hsieh, MSc in Management and Business Analytics,",
                                                                 a(href="mailto:laurene.hsieh@unil.ch", "laurene.hsieh@unil.ch")))),
                                              tags$li(HTML(paste("Visesa Jain, MSc in Management,",
                                                                 a(href="mailto:visesa.jain@unil.ch", "visesa.jain@unil.ch")))),
                                              tags$li(HTML(paste("Aellya Monney, MSc in Management and Business Analytics,",
                                                                 a(href="mailto:aellya.monney@unil.ch", "aellya.monney@unil.ch")))),
                                              tags$li(HTML(paste("Corinne Schonholzer, MSc in Accounting, Control and Finance,",
                                                                 a(href="mailto:corinne.schonholzer@unil.ch", "corinne.schonholzer@unil.ch")))),
                                              tags$li(HTML(paste("Remy Tombola, MSc in Management and Business Analytics,",
                                                                 a(href="mailto:remy.tombola@unil.ch", "remy.tombola@unil.ch")))),
                                              shiny::br(),
                                              shiny::br(),
                                              div(class=".logo",img(src="assets/logohec.png",    # HEC logo
                                                                    height="20%",
                                                                    width="20%"))
                                            ) #sidebarPanel
                                          ) #sidebarLayout
                          ) #tabPanel


  ) #navbarPage END OF UI




  server <- function(input, output, session) {

    #-----------------------------------------------------------------------------
    # Page 1 : EU Map
    #-----------------------------------------------------------------------------

    # for display date under the title Global Summary
    formatted_date <- reactive({

      as.POSIXct(input$plot_date)

    })

    output$clean_date_reactive <- shiny::renderText({
      format(formatted_date(), "%d %B %Y")
    })

    # reactive_db to input$plot_date for Global summary
    reactive_db <- reactive({

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
        dplyr::mutate(popup_info = paste(strong(toupper(`location`)), "<br/>",
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
                           labFormat = labelFormat(suffix = " %"))

    }) #renderLeaflet

    #-----------------------------------------------------------------------------
    # Page Data
    #-----------------------------------------------------------------------------

    # Display Dataset
    output$data <- DT::renderDT({
      database_EU %>%
        dplyr::relocate(people_fully_vaccinated_per_hundred_MA,
                        .after = people_fully_vaccinated_per_hundred) %>%
        datatable(rownames = FALSE, # remove index column
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
        dygraphs::dyRangeSelector(fillColor = rgb(0.85,0.85,0.93,0.8)) %>%
        dygraphs::dyHighlight(highlightCircleSize = 5) %>%
        dygraphs::dySeries(color = "darkcyan") %>%
        dygraphs::dyOptions(fillGraph = TRUE # fill area
                            ,drawGrid = FALSE # remove grid
                            ,labelsKMB = "K")      # change y axis format
    }) #renderDygraph


    # REACTIVE DATASET for barplots
    data1 <- reactive({

      database_EU %>%
        dplyr::group_by(location) %>%
        dplyr::slice(which.max(as.Date(date, '%d/%m/%Y'))) # select row with most recent date by country (location)

    }) #reactive


    # BARPLOT - TOTAL CASES
    output$barplot_cases <- plotly::renderPlotly({

      plotly::ggplotly(
        ggplot(data1(),
               aes(x = reorder(location, -total_cases),  # order bar plot by total cases
                   y = total_cases,
                   text = paste('</br><b>Country:</b> ', location,
                                '</br><b>Total cases:</b> ', comprss(total_cases),
                                '</br><b>Date:</b> ', date))) +
          geom_bar(stat = "identity", fill = "dodgerblue4", alpha = 0.8) +
          theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) + #to adjust label axis for better visibility
          scale_y_continuous(labels = scales::label_number_si()) +  # to format total cases numbers into Millions
          xlab("") +
          ylab("Total Cases") +
          ggtitle("") +
          theme(legend.position = "none",
                panel.background = element_rect(fill = "#ffffff00",
                                                color = "#ffffff00"), # make plot background transparent
                plot.background = element_rect(fill = "#ffffff00",
                                               color = "#ffffff00")),
        tooltip = "text")  #to only have text in the tooltip

    }) #renderPlotly


    # BARPLOT - TOTAL CASES PER MILLION
    output$barplot_cases_m <- plotly::renderPlotly({

      plotly::ggplotly(
        ggplot(data1(),
               aes(x = reorder(location, -total_cases_per_million), # order bar plot by total cases
                   y = total_cases_per_million,
                   text = paste('</br><b>Country:</b> ', location,
                                '</br><b>Total cases per million:</b> ', comma(total_cases_per_million),
                                '</br><b>Date:</b> ', date))) +
          geom_bar(stat = "identity", fill = "dodgerblue4", alpha = 0.7) +
          theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
          scale_y_continuous(labels = scales::label_number_si()) +
          xlab("") +
          ylab("Total Cases per Million") +
          ggtitle("") +
          theme(legend.position = "none",
                panel.background = element_rect(fill = "#ffffff00",
                                                color = "#ffffff00"),   # make plot background transparent
                plot.background = element_rect(fill = "#ffffff00",
                                               color = "#ffffff00")),
        tooltip = "text")  #to only have text in the tooltip

    }) #renderPlotly


    # BARPLOT - TOTAL DEATHS
    output$barplot_deaths <- plotly::renderPlotly({

      plotly::ggplotly(
        ggplot(data1(),
               aes(x = reorder(location, -total_deaths), # order bar plot by total vacc
                   y = total_deaths,
                   text = paste('</br><b>Country:</b> ', location,
                                '</br><b>Total Death:</b> ', comma(total_deaths),
                                '</br><b>Date:</b> ', date))) +
          geom_bar(stat = "identity", fill = "darkcyan") +
          theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
          scale_y_continuous(labels = scales::label_number_si()) +
          xlab("") +
          ylab("Total Deaths") +
          ggtitle("") +
          theme(legend.position = "none",
                panel.background = element_rect(fill = "#ffffff00",
                                                color = "#ffffff00"),   # make plot background transparent
                plot.background = element_rect(fill = "#ffffff00",
                                               color = "#ffffff00")),
        tooltip = "text")  #to only have text in the tooltip

    }) #renderPlotly


    # BARPLOT - TOTAL DEATHS PER MILLION
    output$barplot_deaths_m <- plotly::renderPlotly({

      plotly::ggplotly(
        ggplot(data1(),
               aes(x = reorder(location, -total_deaths_per_million), # order bar plot by total vacc
                   y = total_deaths_per_million,
                   text = paste('</br><b>Country:</b> ', location,
                                '</br><b>Total Deaths per Million:</b> ', comma(total_deaths_per_million),
                                '</br><b>Date:</b> ', date))) +
          geom_bar(stat = "identity", fill = "darkcyan") +
          theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
          scale_y_continuous(labels = scales::label_number_si()) +
          xlab("") +
          ylab("Total Deaths per Million") +
          ggtitle("") +
          theme(legend.position = "none",
                panel.background = element_rect(fill = "#ffffff00",
                                                color = "#ffffff00"),   # make plot background transparent
                plot.background = element_rect(fill = "#ffffff00",
                                               color = "#ffffff00")),
        tooltip = "text")  #to only have text in the tooltip

    }) #renderPlotly


    # BARPLOT - VACCINES
    output$barplot_vaccines <- plotly::renderPlotly({

      plotly::ggplotly(
        ggplot(data1(),
               aes(x = reorder(location, -people_fully_vaccinated_per_hundred), # order bar plot by total vacc
                   y = people_fully_vaccinated_per_hundred,
                   text = paste('</br><b>Country:</b> ', location,
                                '</br><b>Vaccination Rate:</b> ', paste( people_fully_vaccinated_per_hundred, "%"),
                                '</br><b>Date:</b> ', date))) +
          geom_bar(stat = "identity", fill = "skyblue4") +
          theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
          xlab("") +
          ylab("Vaccination Rate (%)") +
          ggtitle("") +
          theme(legend.position = "none",
                panel.background = element_rect(fill = "#ffffff00",
                                                color = "#ffffff00"),   # make plot background transparent
                plot.background = element_rect(fill = "#ffffff00",
                                               color = "#ffffff00")),
        tooltip = "text")  #to only have text in the tooltip

    }) #renderPlotly

    # BUBBLE PLOT
    output$bubble <- plotly::renderPlotly({

      p1 <- ggplot(data1(),   # use filtered data on most recent date
                   aes(x = gdp_per_capita,
                       y = people_fully_vaccinated_per_hundred,
                       size = total_cases_per_million, #population
                       color = total_cases_per_million,
                       text = paste('</br><b>Country:</b> ', location,
                                    '</br><b>Total Cases per Million:</b> ', comma(round(total_cases_per_million)),
                                    '</br><b>Vaccination Rate:</b> ', paste(people_fully_vaccinated_per_hundred,"%"),
                                    '</br><b>Population:</b> ', comma(population),
                                    '</br><b>GDP per Capita:</b> ', paste(comma(gdp_per_capita),"$")))) +
        geom_point(alpha = 0.8) +
        scale_size(range = c(.1, 10), name ="Population (M)")  +
        scale_color_viridis(alpha = 0.6,
                            option="viridis",
                            label = scales::comma) +
        scale_x_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
        ylab("Vaccination Rate (%)") +
        xlab("GDP per Capita ($)") +
        labs(color = "Total Cases per Million\n") +
        theme(panel.background = element_rect(fill = "#ffffff00",
                                              color = "#ffffff00"),   # make plot background transparent
              plot.background = element_rect(fill = "#ffffff00",
                                             color = "#ffffff00"))
      plotly::ggplotly(p1, tooltip = "text") %>%
        layout(xaxis = list(autorange = TRUE),
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
    data2 <- reactive({

      req(input$country)
      database_EU %>%
        dplyr::filter(location %in% input$country) # filter the location selected by the user

    }) #reactive


    # COLOR PALETTE FOR THE FOLLOWING PLOTS
    cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


    # CASES PER DAY COMPARATOR
    output$comparator_1 <- plotly::renderPlotly({

      plotly::ggplotly(
        ggplot(data2(),
               aes(x = date,
                   y = new_cases_smoothed,
                   color = location,
                   group = 1,
                   text = paste('</br><b>Country:</b> ', location,
                                '</br><b>New cases (smoothed):</b> ', comprss(new_cases_smoothed),
                                '</br><b>Date:</b> ', date))) +
          geom_line() +
          scale_colour_manual(values=cbPalette) +
          scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
          theme(legend.position = "none") +
          labs(x = "Date",
               y = "New Cases") +
          theme(legend.position = "none",
                panel.background = element_rect(fill = "#ffffff00",
                                                color = "#ffffff00"),   # make plot background transparent
                plot.background = element_rect(fill = "#ffffff00",
                                               color = "#ffffff00"))
        ,tooltip = "text")

    }) #renderPlotly


    # CASES PER DAY PER MILLION COMPARATOR
    output$comparator_1bis <- plotly::renderPlotly({

      plotly::ggplotly(
        ggplot(data2(),
               aes(x = date,
                   y = new_cases_smoothed_per_million,
                   color = location,
                   group = 1,
                   text = paste('</br><b>Country:</b> ', location,
                                '</br><b>New cases per million (smoothed):</b> ', comma(new_cases_smoothed_per_million,2),
                                '</br><b>Date:</b> ', date))) +
          geom_line() +
          scale_colour_manual(values=cbPalette) +
          scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
          theme(legend.position = "none") +
          labs(x = "Date",
               y = "New Cases per Million") +
          theme(legend.position = "none",
                panel.background = element_rect(fill = "#ffffff00",
                                                color = "#ffffff00"),   # make plot background transparent
                plot.background = element_rect(fill = "#ffffff00",
                                               color = "#ffffff00")),
        tooltip = "text")

    }) #renderPlotly


    # VACCINATION PER DAY COMPARATOR
    output$comparator_2 <- plotly::renderPlotly({

      plotly::ggplotly(
        ggplot(data2(),
               aes(x = date,
                   y = people_fully_vaccinated_per_hundred_MA,
                   color = location,
                   group = 1,
                   text = paste('</br><b>Country:</b> ', location,
                                '</br><b>Vaccination Rate:</b> ', paste(comprss(people_fully_vaccinated_per_hundred_MA), "%"),
                                '</br><b>Date:</b> ', date))) +
          geom_line(na.rm = TRUE) +
          scale_colour_manual(values = cbPalette) +
          scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
          theme(legend.position = "none") +
          labs(x = "Date",
               y = "Vaccination Rate (%)") +
          theme(legend.position = "none",
                panel.background = element_rect(fill = "#ffffff00",
                                                color = "#ffffff00"),   # make plot background transparent
                plot.background = element_rect(fill = "#ffffff00",
                                               color = "#ffffff00"))
        ,tooltip = "text")

    }) #renderPlotly

    # REACTIVE DATASET FOR BARPLOT COMPARATOR
    data3 <- reactive({

      req(input$country)

      database_EU %>%
        dplyr::group_by(location) %>%
        dplyr::slice(which.max(as.Date(date, '%d/%m/%Y'))) %>%
        dplyr::filter(location %in% input$country)

    }) #reactive

    # BARPLOT STRINGENCY INDEX
    output$comparator_barplot_si <- plotly::renderPlotly({

      p <-  data3() %>%
        ggplot(aes(x = reorder(location, -stringency_index), # reorder barplots
                   y = stringency_index,
                   fill = input$country,
                   text = paste('</br><b>Country:</b> ', location,
                                '</br><b>Stringency index:</b> ', stringency_index,
                                '</br><b>Date:</b> ', date))) +
        geom_bar(stat = "identity", width = 0.5) +
        scale_fill_manual(values=cbPalette) +
        xlab("") +
        ylab("Current Stringency Index") +
        ggtitle("") +
        theme(legend.position = "none",
              panel.background = element_rect(fill = "#ffffff00",
                                              color = "#ffffff00"),   # make plot background transparent
              plot.background = element_rect(fill = "#ffffff00",
                                             color = "#ffffff00"))

      p %>% plotly::ggplotly(tooltip = "text")

    }) #renderPlotly


    # BARPLOT MEDIAN AGE
    output$comparator_barplot_median_age <- plotly::renderPlotly({

      p2 <-  data3() %>%
        ggplot(aes(x = reorder(location, -median_age), # order bar plot by total vacc
                   y = median_age,
                   fill = input$country,
                   text = paste('</br><b>Country:</b> ', location,
                                '</br><b>Median Age:</b> ', median_age,
                                '</br><b>Date:</b> ', date))) +
        geom_bar(stat = "identity", width = 0.5) +
        scale_fill_manual(values=cbPalette) +
        xlab("") +
        ylab("Median Age") +
        ggtitle("") +
        theme(legend.position = "none",
              panel.background = element_rect(fill = "#ffffff00",
                                              color = "#ffffff00"),   # make plot background transparent
              plot.background = element_rect(fill = "#ffffff00",
                                             color = "#ffffff00"))

      p2 %>% plotly::ggplotly(tooltip = "text")


    }) #renderPlotly

    # BARPLOT DEATH
    output$comparator_barplot_death <- plotly::renderPlotly({

      p3 <-  data3() %>%
        ggplot(aes(x = reorder(location, -new_deaths_smoothed), # order bar plot by total vacc
                   y = new_deaths_smoothed,
                   fill = input$country,
                   text = paste('</br><b>Country:</b> ', location,
                                '</br><b>New deaths smoothed:</b> ', new_deaths_smoothed,
                                '</br><b>Date:</b> ', date))) +
        geom_bar(stat = "identity", width = 0.5) +
        scale_fill_manual(values=cbPalette) +
        xlab("") +
        ylab("New Deaths Smoothed") +
        ggtitle("") +
        theme(legend.position = "none",
              panel.background = element_rect(fill = "#ffffff00", color = "#ffffff00"),   # make plot background transparent
              plot.background = element_rect(fill = "#ffffff00", color = "#ffffff00"))

      p3 %>% plotly::ggplotly(tooltip = "text")

    }) #renderPlotly


    # BARPLOT DEATHS PER MILLION
    output$comparator_barplot_death_pm <- plotly::renderPlotly({

      p4 <-  data3() %>%
        ggplot(aes(x = reorder(location, -new_deaths_smoothed_per_million), # order bar plot by total vacc
                   y = new_deaths_smoothed_per_million,
                   fill = input$country,
                   text = paste('</br><b>Country:</b> ', location,
                                '</br><b>New deaths smoothed per million:</b> ', new_deaths_smoothed_per_million,
                                '</br><b>Date:</b> ', date))) +
        geom_bar(stat = "identity", width = 0.5) +
        scale_fill_manual(values=cbPalette) +
        # coord_flip() +
        # theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
        xlab("") +
        ylab("New Deaths Smoothed per Million") +
        ggtitle("") +
        theme(legend.position = "none",
              panel.background = element_rect(fill = "#ffffff00", color = "#ffffff00"),   # make plot background transparent
              plot.background = element_rect(fill = "#ffffff00", color = "#ffffff00"))

      p4 %>% plotly::ggplotly(tooltip = "text")


    }) #renderPlotly


    # BARPLOT CARDIOVASC.
    output$comparator_barplot_cardio <- plotly::renderPlotly({

      p <-  data3() %>%
        ggplot(
          aes(x = reorder(location, -cardiovasc_death_rate), # reorder barplots
              y = cardiovasc_death_rate,
              fill = input$country,
              text = paste('</br><b>Country:</b> ', location,
                           '</br><b>Cardiovascular Death Rate:</b> ', cardiovasc_death_rate,
                           '</br><b>Date:</b> ', date))) +
        geom_bar(stat = "identity", width = 0.5) +
        scale_fill_manual(values=cbPalette) +
        xlab("") +
        ylab("Cardiovascular Death Rate") +
        ggtitle("") +
        theme(legend.position = "none",
              panel.background = element_rect(fill = "#ffffff00",
                                              color = "#ffffff00"),   # make plot background transparent
              plot.background = element_rect(fill = "#ffffff00",
                                             color = "#ffffff00"))

      p %>% plotly::ggplotly(tooltip = "text")

    }) #renderPlotly

    # BARPLOT DIABETES
    output$comparator_barplot_diabetes <- plotly::renderPlotly({

      p <-  data3() %>%
        ggplot(aes(x = reorder(location, -diabetes_prevalence), # reorder barplots
                   y = diabetes_prevalence,
                   fill = input$country,
                   text = paste('</br><b>Country:</b> ', location,
                                '</br><b>Diabetes Prevalence:</b> ', diabetes_prevalence,
                                '</br><b>Date:</b> ', date))) +
        geom_bar(stat = "identity", width = 0.5) +
        scale_fill_manual(values=cbPalette) +
        xlab("") +
        ylab("Diabetes Prevalence") +
        ggtitle("") +
        theme(legend.position = "none",
              panel.background = element_rect(fill = "#ffffff00",
                                              color = "#ffffff00"),   # make plot background transparent
              plot.background = element_rect(fill = "#ffffff00",
                                             color = "#ffffff00"))

      p %>% plotly::ggplotly(tooltip = "text")

    }) #renderPlotly


  } # SERVER END



  ################ APPLICATION ##################
  # make app appear
  shinyApp(ui = ui, server = server)
}
