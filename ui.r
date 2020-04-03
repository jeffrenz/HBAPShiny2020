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
  #tags$head(HTML("<title>HBAP Team Shiny</title>")), #no logo
  tags$head(HTML("<title>HBAP Team Shiny</title> <link rel='icon' href='https://www.shinyapps.dev/img/icon-192.png'>")), #With logo
  titlePanel(
    fluidRow(
      column(1,
             fluidRow(
               img(src='CoronavirusTitleOrange.jpg', align = "center", width = "300")
             )
      ),
      column(3,offset = 1,
             fluidRow(
               column(1,offset = 1,
               ),
               column(10,
                      fluidRow(
                        textOutput("global_header"),
                        tags$style(type="text/css", "#global_header {font-family: 'Bradley Hand ITC'; font-size:28px;font-weight: bold;color:darkorange}")
                        )
                      )
               ),
             fluidRow(
               column(1,offset = 2,
                      fluidRow(
                        img(src='earthMask.jpg', align = "right", width="60px")
                      )
               ),
               column(1,
                      fluidRow(
                        textOutput("global_cases"),
                        tags$style(type="text/css", "#global_cases {font-family: 'times'; font-size:20px;color:black}")
                      ),
                      fluidRow(
                        p("Cases", style = "font-family: 'times'; font-size:12px;color:black")
                      )
               ),
               column(2,offset=2,
                      fluidRow(
                        textOutput("global_deaths"),
                        tags$style(type="text/css", "#global_deaths {font-family: 'times'; font-size:20px;color:red}")
                      ),
                      fluidRow(
                        p("Deaths", style = "font-family: 'times'; font-size:12px;color:red")
                      )
               ), 
               column(2,offset=1,
                      fluidRow(
                        textOutput("global_cfr"),
                        tags$style(type="text/css", "#global_cfr {font-family: 'times'; font-size:20px;color:black}")
                      ),
                      fluidRow(
                        p("CFR", style = "font-family: 'times'; font-size:12px;color:black")
                        )
                      )
               )
             ),
      column(3,
             fluidRow(
               column(1,
               ),
               column(10,
                      fluidRow(
                        textOutput("selected_country_total"),
                        tags$style(type="text/css", "#selected_country_total {font-family: 'Bradley Hand ITC'; font-size:28px;font-weight: bold;color:darkorange}")
                        )
                      )
               ),
             fluidRow
             (
               column(1,offset=1,
                      fluidRow(
                        img(src='emoji-surgical-mask.png', align = "right", width="50px")
                        )
                      ),
               column(1,
                       fluidRow(
                         textOutput("country_cases_total"),
                         tags$style(type="text/css", "#country_cases_total {font-family: 'times'; font-size:20px;color:black}")
                         ),
                      fluidRow(
                        p("Cases", style = "font-family: 'times'; font-size:12px;color:black")
                        )
                      ),
               column(1,offset = 2,
                      fluidRow(
                        textOutput("country_deaths_total"),
                        tags$style(type="text/css", "#country_deaths_total {font-family: 'times'; font-size:20px;color:red}")
                        ),
                      fluidRow(
                        p("Deaths", style = "font-family: 'times'; font-size:12px;color:red")
                        )
                      ),
               column(1,offset = 2,
                        fluidRow(
                          textOutput("country_recovered_cfr"),
                          tags$style(type="text/css", "#country_recovered_cfr {font-family: 'times'; font-size:20px;color:black}")
                          ),
                        fluidRow(
                          p("CFR", style = "font-family: 'times'; font-size:12px;color:black")
                          )
                        )
               )
             ),
      column(2,
             fluidRow(
               column(1,
                      fluidRow(
                        img(src='HBAP.jpg', align = "left",width = "80px", height = "auto")
                        )
                      ),
               column(6,offset = 4,
                      fluidRow(
                        p("Team Shiny", style = "font-family: 'times'; font-size:20px;color:black"),
                        h6("Jeff Renz", br(),"Itauma Itauma",br(),"Ken Cutt")
                        )
                      )
               )
             ),
      )
    ),
  
  sidebarPanel(
    fluidRow(
      selectInput("varCounty",
                  label = "Choose a Country",
                  choices = countries,
                  selected = "US")
      ),
    fluidRow(
      # Input: Slider for the number of observations to generate ----
      sliderInput("n",
                  "Trend: Observations",
                  value = 14,
                  min = 10,
                  max = 60
                  )
    ),
    fluidRow(
      withLoader(plotOutput("covid_plot_by_cases"))
    ),
    fluidRow(
      textOutput("global_reportdate")
      )
    ,width=2),
  
  mainPanel(
    fluidRow(
      tabsetPanel(
        # using iframe along with tags() within tab to display pdf with scroll, height and width could be adjusted
        tabPanel("World Map",
                 leafletOutput("map", width = "auto", height =  725)
        ),
        tabPanel("By The Numbers",
                 br(),
                 DT::dataTableOutput("current_stats_by_state_province_table",width = "700px")
                 #withLoader(htmlOutput("filetable"))
                 ),
        tabPanel("Trend",
                 br(),
                 withLoader(plotOutput("covid_plot_by_country",width = "1000px", height = "500px"))
                ),
        tabPanel("What I Should Know",
                 # tags$iframe(style="height:700px; width:1000px; scrolling=yes",
                 #             src="https://www.cdc.gov/coronavirus/2019-ncov/downloads/2019-ncov-factsheet.pdf")
               img(src='coronavirus_what_I_should_know.png', align = "left")
               ),
        tabPanel("Covid-19 vs The Flu",
                 tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/FVIGhz3uwuQ", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA)
        ),
        tabPanel("Why Flatten The Curve?",
                 #htmlOutput("nytimes_iframe"),
               img(src='flattening-the-curve_orig.png', align = "left",width = "auto", height = "600px")
               ),
      
        tabPanel("Situation Report (WHO)",
                 tags$iframe(style="height:700px; width:1000px; scrolling=yes", 
                             src=situatation_report_pdf),
                 textOutput("situation_report_name")
                 ),
        tabPanel("Health Map (Kinsa)",
                 htmlOutput("kinsa_iframe")
                 )
      ),
      )    
    , width=10)
)