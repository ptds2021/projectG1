#############################################
## The following loads the needed packages ##
#############################################

# load the required packages
packages <- c(
  "here",  # for organization
  "summarytools",      # data summary
  "questionr",         # EDA
  "xts","tsbox","dygraphs", # dygraphs
  "formattable", "scales",    # number formats
  "ggplot2","gridExtra","plotly", "dygraphs", "leaflet", "viridis", # for visualization
  "tidyverse", "lubridate", "dplyr", "zoo",# for wrangling
  "shiny", "dashboardthemes","shinydashboard", "DT", "shinyWidgets", "htmltools", "shinythemes",      # Shiny
  "shinycssloaders",
  "knitr", "kableExtra", "bookdown","haven", "rmarkdown","kableExtra", "xaringan" # for the report
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

# knitr options
#opts_chunk$set(
#  comment = "#>")
#  collapse = TRUE,
#  cache = TRUE,
#  fig.retina = 0.8, # figures are either vectors or 300 dpi diagrams
#  dpi = 300,
#  out.width = "70%",
#  fig.align = "center",
#  fig.width = 6,
#  fig.asp = 0.618,
#  fig.show = "hold",
#  message = FALSE,
#  echo = FALSE
#)

#kable options
kable <- function(data, ...) {
  knitr::kable(data,
               booktabs = TRUE,
               col.names = gsub("[_]", " ",names(data)), ...) %>%
    kable_styling(bootstrap_options =c("striped", "hover"),
                  full_width = T,
                  position = "left",
                  fixed_thead = T)
}


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





######################################################
##      The following sets the dashboard theme     ##
######################################################

customTheme <- shinyDashboardThemeDIY(

  ### general
  appFontFamily = "Verdana"
  ,appFontColor = "rgb(0,0,0)" #black
  #,primaryFontColor = "rgb(255, 20, 147)" ROSE FLASH
  #,infoFontColor = "rgb(0,0,0)"
  #,successFontColor = "rgb(0,0,0)"
  #,warningFontColor = "rgb(255, 20, 147)" #ROSE FLASH
  #,dangerFontColor = "rgb(0,0,0)"
  ,bodyBackColor = "rgb(255, 255, 255)" #noir pour le moment mais en blanc après 255,255,255

  ### header
  ,logoBackColor = "rgb(255,255,255,255)"
  ,headerButtonBackColor = "rgb(255,255,255)" #slider button
  ,headerButtonIconColor = "rgb(16,78,139)" #icon dans slider button pour cacher la sidebar
  ,headerButtonBackColorHover = cssGradientThreeColors(
    direction = "down"
    ,colorStart = "rgb(255,255,255)"
    ,colorMiddle = ""
    ,colorEnd = "rgba(16,78,139)"
    ,colorStartPos = 0
    ,colorMiddlePos = 10
    ,colorEndPos = 100
  )#quand la souris va sur le slider button
  ,headerButtonIconColorHover = "rgb(255,255,255)"

  ,headerBackColor = "rgb(255,255,255)" #barre à coté du logo
  ,headerBoxShadowColor = "#aaaaaa"
  ,headerBoxShadowSize = "2px 2px 2px"

  ### sidebar
  ,sidebarBackColor = cssGradientThreeColors(
    direction = "down"
    ,colorStart = "rgb(255,255,255)"
    ,colorMiddle = ""
    ,colorEnd = "rgb(255,255,255)"
    ,colorStartPos = 0
    ,colorMiddlePos = 50
    ,colorEndPos = 100
  )
  ,sidebarPadding = 0

  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0

  ,sidebarShadowRadius = "3px 5px 5px"
  ,sidebarShadowColor = "#aaaaaa"

  ,sidebarUserTextColor = "rgb(255, 20, 147)" #!!!ROSE!! mais je vois pas où est-ce que c'est

  ,sidebarSearchBackColor = "rgb(255,255,255)"  #button upload A REVOIR
  ,sidebarSearchIconColor = "rgb(0,0,0)" #button upload
  ,sidebarSearchBorderColor = "rgb(255,255,255)" #button upload

  ,sidebarTabTextColor = "rgb(16,78,139)" #texte sidebar blanc
  ,sidebarTabTextSize = 13
  ,sidebarTabBorderStyle = "none none none none"
  ,sidebarTabBorderColor = "rgb(16,78,139)"
  ,sidebarTabBorderWidth = 1

  ,sidebarTabBackColorSelected = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgb(255,255,255)"
    ,colorMiddle = ""
    ,colorEnd = "rgba(16,78,139)"
    ,colorStartPos = 0
    ,colorMiddlePos = 20
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorSelected = "rgb(255,255,255)"
  ,sidebarTabRadiusSelected = "0px 20px 20px 0px"

  ,sidebarTabBackColorHover = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgb(255,255,255)"
    ,colorMiddle = ""
    ,colorEnd = "rgba(16,78,139)"
    ,colorStartPos = 0
    ,colorMiddlePos = 20
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorHover = "rgb(255,255,255)"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "rgb(255,255,255)" #blanc
  ,sidebarTabBorderWidthHover = 1
  ,sidebarTabRadiusHover = "0px 20px 20px 0px"

  ### boxes dans charts à modif
  ,boxBackColor = "rgb(242,242,242)" #gris
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 1px 1px"
  ,boxShadowColor = "rgba(0,0,0,.1)"
  ,boxTitleSize = 16
  ,boxDefaultColor = "rgb(210,214,220)"
  ,boxPrimaryColor = "rgb(255, 20, 147)" #pink "rgb(255, 20, 147)" #pink
  ,boxInfoColor = "rgb(0,100,0)" #green "rgb(0,100,0)" #green
  ,boxSuccessColor = "rgb(16,78,139)" #upload success
  ,boxWarningColor = "rgb(244,156,104)"
  ,boxDangerColor = "rgb(255,88,55)"

  ,tabBoxTabColor = "rgb(255,255,255)" #blanc
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(255, 20, 147)" #rose
  ,tabBoxTabTextColorSelected = "rgb(0,0,0)" #noir
  ,tabBoxBackColor = "rgb(0,100,0)" #vert
  ,tabBoxHighlightColor = "rgb(139,0,0)" #rouge
  ,tabBoxBorderRadius = 5

  ### inputs à modifier je ne vois pas où c'est
  ,buttonBackColor = "rgb(245,245,245)" #select all button back
  ,buttonTextColor = "rgb(0,0,0)" #select all button back
  ,buttonBorderColor = "rgb(200,200,200)" #
  ,buttonBorderRadius = 5

  ,buttonBackColorHover = "rgb(235,235,235)" #hover of the select all button
  ,buttonTextColorHover = "rgb(100,100,100)"
  ,buttonBorderColorHover = "rgb(200,200,200)"

  ,textboxBackColor = "rgb(255,255,255)" #blanc
  ,textboxBorderColor = "rgb(200,200,200)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(245,245,245)"
  ,textboxBorderColorSelect = "rgb(200,200,200)"

  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(242,242,242)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1

)

customLogo <- shinyDashboardLogoDIY(

  boldText = ""
  ,mainText = ""
  ,textSize = 1
  ,badgeText = "COVID-19 Tracker"
  ,badgeTextColor = "rgba(16,78,139)"
  ,badgeTextSize = 5
  ,badgeBackColor = "rgb(255,255,255)"
  ,badgeBorderRadius = 5

)
