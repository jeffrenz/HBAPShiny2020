library(shiny)
library(ggplot2)
library(htmlTable)
library(scales)

library(leaflet)
library(spData)
library(dplyr)
library(ggmap)

library(RODBC)
library(odbc)

library(stringr)
library(httr)

library(DT)
library(shinycustomloader)

# Run functions.r script to load
rel_path_from_root <- "scripts/Functions.r"
source(rel_path_from_root)

#data for side panel
countries <-get_countries()
g_stats <- get_global_stats()
global_cases <- comma(g_stats$TotalConfirmed)
global_deaths <- comma(g_stats$TotalDeaths)
global_recovered <- comma(g_stats$TotalRecovered)

fluidPage(
  titlePanel(
    fluidRow(
      column(3,
             img(src='CoronavirusTitleOrange.jpg', align = "center", width = "400"),
      ),
      column(7,
             fluidRow(
               textOutput("selected_country_total"),
               tags$style(type="text/css", "#selected_country_total {font-family: 'Bradley Hand ITC'; font-size:50px;font-weight: bold;color:darkorange}"),
             ),
             fluidRow
             (
                 column(1,
                        img(src='emoji-surgical-mask.png', align = "right", width="50px")
                 ),
                 column(1,
                       fluidRow(
                       textOutput("country_cases_total"),
                       tags$style(type="text/css", "#country_cases_total {font-family: 'times'; font-size:28px;color:black}"),
                       ),
                       fluidRow(
                       p("Cases", style = "font-family: 'times'; font-size:12px;color:black"),
                        ),
                       ),
                 column(1,
                        img(src='recovered.png', align = "right", width="50px")
                 ),
                 column(1,
                        fluidRow(
                          textOutput("country_recovered_total"),
                          tags$style(type="text/css", "#country_recovered_total {font-family: 'times'; font-size:28px;color:black}"),
                        ),
                        fluidRow(
                          p("Recovered", style = "font-family: 'times'; font-size:12px;color:black"),
                        ),
                 ),
                 column(1,
                        img(src='death.png', align = "right", width = "50px")
                 ),
                 column(1,
                        fluidRow(
                          textOutput("country_deaths_total"),
                          tags$style(type="text/css", "#country_deaths_total {font-family: 'times'; font-size:28px;color:black}"),
                        ),
                        fluidRow(
                          p("Deaths", style = "font-family: 'times'; font-size:12px;color:black"),
                        ),
                 ),
               ),
      ),
    ),
  ),
  sidebarPanel(
    fluidRow(
              selectInput("varCounty", 
              label = "Choose a country",
              choices = countries,
              selected = "US"),
    ),
    fluidRow(
        withLoader(plotOutput("covid_plot_by_cases")),
    ),
    fluidRow(
      br(),
    wellPanel(
      h4("HBAP Team Shiny"),
      h6("Jeff Renz, Itauma Itauma and Ken Cutt")
    ),
    )
  , width=2),
    
  mainPanel(
    fluidRow(
      tabsetPanel(
        # using iframe along with tags() within tab to display pdf with scroll, height and width could be adjusted
        tabPanel("World Map",
                 leafletOutput("map", width = "auto", height =  625),
        ),
        tabPanel("By The Numbers",
                 br(),
                 withLoader(htmlOutput("filetable"))),
        tabPanel("Trend",
                 br(),
                withLoader(plotOutput("covid_plot_by_country",width = "800px", height = "600px"))
                ),  
        tabPanel("Situation Report", 
                  tags$iframe(style="height:700px; width:100%; scrolling=yes", 
                              src=situatation_report_pdf)
                 ),
      tabPanel("What I Should Know",
               img(src='Coronavirus_usa_today.png', align = "left"),
               ),
      tabPanel("Why Flatten The Curve?",
               img(src='flattening-the-curve_orig.png', align = "left",width = "auto", height = "600px"),
      )
      
    ),
    )    
    , width=10)
)