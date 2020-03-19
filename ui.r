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


#data for side panel
countries <-get_countries()

fluidPage(
  
  titlePanel(h1("HBAP Team Shiny")),

  #titlePanel(color = "blue", title = "HBAP Team Shiny", inverted = TRUE)
  sidebarPanel(
    fluidRow(
    img(src='Coronavirus_header.jfif', align = "left", width = "575px"),
    br()
    ),
    fluidRow(
      column(width = 12,
        br(),
        p("Globally", style = "font-family: 'times'; font-size:30px;color:darkorange"),
        fluidRow(
            column(width =12,
                   fluidRow
                   (
                     column(width =2,
                            img(src='emoji-surgical-mask.png', align = "left", width="50px")
                     ),               
                     column(width =2,
                            fluidRow(       
                              p(global_cases, style = "font-family: 'times'; font-size:24px;color:black"),
                              p("Cases", style = "font-family: 'times'; font-size:12px;color:black"),
                            )
                     ),
                     column(width =2,
                            img(src='recovered.png', align = "left", width="50px")              
                     ),               
                     column(width =2,
                            fluidRow(       
                              p(global_recovered, style = "font-family: 'times'; font-size:24px;color:black"),
                              p("Recovered", style = "font-family: 'times'; font-size:12px;color:black"),
                            ),
                     ),
                     column(width =2,
                            img(src='emoji-dead-emoticon.png', align = "left", width = "50px")              
                     ),               
                     column(width =2,
                            fluidRow(       
                              p(global_deaths, style = "font-family: 'times'; font-size:24px;color:black"),
                              p("Deaths", style = "font-family: 'times'; font-size:12px;color:black"),
                            ),
                     )                     
                   ),
            )
        ),
      )
    ),
    br(),
    selectInput("varCounty", 
                label = "Choose a country",
                choices = countries,
                selected = "US"),
    
    # selectInput("select_country_with_updateSelectInput",
    #             label = "Choose a country SQL: ",
    #             choices = NULL),
    # radioButtons("varDisplay", "Display:",
    #              c("Interactive Map" = "interactive_map",
    #                "History Table" = "table",
    #                "Situation Report" = "who"))     
    
  ),
    
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
        tabPanel("Interactive Map",leafletOutput("map", width = "auto", height =  800)),
        tabPanel("By The Numbers",
                 fluidRow(
                 img(src='Coronavirus.JPG', align = "left", width = 90),
                 textOutput("selected_country_table_header"),
                 tags$head(tags$style("#selected_country_table_header{color: darkorange;
                                     font-size: 40px;
                                     font-family: times;
                                     }"
                 )),
                 ),
                 withLoader(htmlOutput("filetable"))),
        tabPanel("Trend",
                 fluidRow(
                 img(src='Coronavirus.JPG', align = "left", width = 90),
          textOutput("selected_country"),
          tags$head(tags$style("#selected_country{color: darkorange;
                                     font-size: 40px;
                                     font-family: times;
                                     }"
          )),
                #actionButton("update_chart", label = "Update chart", width = "100"),
                 ),
                withLoader(plotOutput("covid_plot_by_country",width = "800px", height = "600px"))
        ),  
        tabPanel("Situation Report", 
                  tags$iframe(style="height:700px; width:100%; scrolling=yes", 
                              src=situatation_report_pdf))
        ),
    ),    
  )
)