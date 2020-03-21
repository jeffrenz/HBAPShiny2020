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
  #titlePanel(h1("HBAP Team Shiny")),
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
                        img(src='emoji-dead-emoticon.png', align = "right", width = "50px")
                 ),
                 column(1,
                        fluidRow(
                          textOutput("country_deaths_total"),
                          tags$style(type="text/css", "#country_deaths_total {font-family: 'times'; font-size:28px;color:black}"),
                        ),
                        fluidRow(
                          p("Death", style = "font-family: 'times'; font-size:12px;color:black"),
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
    )
  , width=2),
    
  mainPanel(
    # fluidRow(
    #   textOutput("selected_country"),
    #   tags$head(tags$style("#selected_country{color: darkorange;
    #                              font-size: 40px;
    #                              font-family: times;
    #                              }"
    #   )),
    # ),
    fluidRow(
      #conditionalPanel(condition = "input.varDisplay == 'who'", img(src='DistributionMap.JPG', align = "center")),
      #conditionalPanel(condition = "input.varDisplay == 'interactive_map'", leafletOutput("map")),
      tabsetPanel(
        # using iframe along with tags() within tab to display pdf with scroll, height and width could be adjusted
        tabPanel("World Map",
                 leafletOutput("map", width = "auto", height =  800),
                 # div(class="outer",
                 #     tags$head(
                 #       # Include our custom CSS
                 #       includeCSS("styles.css"),
                 #       includeScript("gomap.js")
                 #     ),                
                     
                     # If not using custom CSS, set height of leafletOutput to a number instead of percent
                     #leafletOutput("map", width="auto", height=800),                

                     
                     # Shiny versions prior to 0.11 should use class = "modal" instead.
                     # absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                     #               draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                     #               width = 330, height = "auto",                  
                     
                                   

                 #,leafletOutput("map", width = "auto", height =  800)
                 #),
                 #),
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
               #withLoader(plotOutput("covid_plot_by_cases",width = "575px"))
               )
      
    ),
    )    
    , width=10)
)