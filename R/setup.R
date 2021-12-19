#############################################
## The following loads the needed packages ##
#############################################

# load the required packages
packages <- c(
  "here",  # for organization
  "xts","tsbox","dygraphs", # dygraphs
  "formattable", "scales",    # number formats
  "ggplot2","plotly", "leaflet", "viridis", # for visualization
  "tidyverse", "lubridate", "dplyr", "zoo",# for wrangling
  "shiny", "dashboardthemes","shinydashboard", "DT", "shinyWidgets", "htmltools", "shinythemes",      # Shiny
  "shinycssloaders",
  "knitr","haven", "rmarkdown", "xaringan" # for the report
)
purrr::walk(packages, library, character.only = TRUE)

######################################################
## The following sets a few option for nice reports ##
######################################################

# general options
options(
  digits = 6,
  str = strOptions(strict.width = "cut"),
  width = 69,
  tibble.width = 69,
  cli.unicode = FALSE,
  scipen = 999
)

# ggplot options
theme_set(theme_minimal())

# to round the number to K, M,...
comprss <- function(tx) {
  div <- findInterval(as.numeric(gsub("\\,", "", tx)),
                      c(0, 1e3, 1e6, 1e9, 1e12) )  # modify this if negative numbers are possible
  paste(round( as.numeric(gsub("\\,","",tx))/10^(3*(div-1)), 2),
        c("","K","M","B","T")[div] )}

# Separator of thousand function
my_comma <- scales::label_comma(accuracy =0.01, big.mark = "'", decimal.mark = ".")

# to format y axis in Dygraph
FUNC_JSFormatNumber <- "function(x) {return x.toString().replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,')}"


