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


# World Health Organization Situation Report
situatation_report_pdf <- get_situatation_report_pdf()

fluidPage(
  titlePanel(
    fluidRow(
      column(3,
             img(src='CoronavirusTitleOrange.jpg', align = "center", width = "400"),
      ),
      column(3,
             fluidRow(
               textOutput("global_header"),
               tags$style(type="text/css", "#global_header {font-family: 'Bradley Hand ITC'; font-size:50px;font-weight: bold;color:darkorange}"),
             ),
             fluidRow(
               column(1,offset = 1,
                      img(src='earthMask.jpg', align = "right", width="60px")
               ),
               column(1,
                      fluidRow(
                        textOutput("global_cases"),
                        tags$style(type="text/css", "#global_cases {font-family: 'times'; font-size:28px;color:black}"),
                      ),
                      fluidRow(
                        p("Cases", style = "font-family: 'times'; font-size:12px;color:black"),
                      ),
               ),
               column(2,offset=2,
                      fluidRow(
                        textOutput("global_deaths"),
                        tags$style(type="text/css", "#global_deaths {font-family: 'times'; font-size:28px;color:red}"),
                      ),
                      fluidRow(
                        p("Deaths", style = "font-family: 'times'; font-size:12px;color:red"),
                      ),
               ), 
               column(2,offset=1,
                      fluidRow(
                        textOutput("global_cfr"),
                        tags$style(type="text/css", "#global_cfr {font-family: 'times'; font-size:28px;color:black}"),
                      ),
                      fluidRow(
                        p("CFR", style = "font-family: 'times'; font-size:12px;color:black"),
                      ),
               ),
             ),
      ),      
      column(5,
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
               # column(1,offset = 1,
               #        img(src='death.png', align = "right", width = "50px")
               # ),
               column(1,offset = 1,
                      fluidRow(
                        textOutput("country_deaths_total"),
                        tags$style(type="text/css", "#country_deaths_total {font-family: 'times'; font-size:28px;color:red}"),
                      ),
                      fluidRow(
                        p("Deaths", style = "font-family: 'times'; font-size:12px;color:red"),
                      ),
               ),               
                 # column(1,offset = 1,
                 #        img(src='percent.jpg', align = "right", width="50px")
                 # ),
                 column(1,offset = 1,
                        fluidRow(
                          textOutput("country_recovered_cfr"),
                          tags$style(type="text/css", "#country_recovered_cfr {font-family: 'times'; font-size:28px;color:black}"),
                        ),
                        fluidRow(
                          p("CFR", style = "font-family: 'times'; font-size:12px;color:black"),
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
      # Input: Slider for the number of observations to generate ----
      sliderInput("n",
                  "Trend: Observations",
                  value = 14,
                  min = 10,
                  max = 60)
      
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
                 leafletOutput("map", width = "auto", height =  750),
        ),
        tabPanel("By The Numbers",
                 br(),
                 DT::dataTableOutput("current_stats_by_state_province_table")
                 #withLoader(htmlOutput("filetable"))
                 ),
        tabPanel("Trend",
                 br(),
                 withLoader(plotOutput("covid_plot_by_country",width = "1000px", height = "500px"))
                ),  
        tabPanel("Situation Report (WHO)",
                  tags$iframe(style="height:625px; width:75%; scrolling=yes", 
                              src=situatation_report_pdf),
                 textOutput("situation_report_name"),
                 ),
        tabPanel("Health Map (Kinsa)",
                 htmlOutput("kinsa_iframe"),
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