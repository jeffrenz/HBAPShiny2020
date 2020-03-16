library(shiny)
library(ggplot2)
library(htmlTable)
library(scales)

library(leaflet)
# library(spData)
library(dplyr)
library(ggmap)

# library(RODBC)
library(stringr)
library(httr)

library(kableExtra)

# Run functions.r script to load
# rel_path_from_root <- "scripts/Functions.r"
# source(rel_path_from_root)

#Global Data
# g_stats <- get_global_stats()
# global_cases <- comma(g_stats$TotalConfirmed)
# global_deaths <- comma(g_stats$TotalDeaths)
# global_recovered <- comma(g_stats$TotalRecovered)

# World Health Organization 
# Let's link the file if needed from the data folder 20200313-sitrep-53-covid-19
situatation_report_pdf <-"https://www.who.int/docs/default-source/coronaviruse/situation-reports/20200313-sitrep-53-covid-19.pdf"

fluidPage(
  
  titlePanel(h1("HBAP Team Shiny")),

  #titlePanel(color = "blue", title = "HBAP Team Shiny", inverted = TRUE)
  sidebarPanel(
    fluidRow(
    img(src='CoronavirusTitle.JPG', align = "left", width = 325),
    br()
    ),
    fluidRow(
      column(width = 12,
        p("Globally", style = "font-family: 'times'; font-size:30px;color:darkorange"),
        fluidRow(
            column(width =12,
                   fluidRow
                   (
                     column(width =2,
                            img(src='Confirmed.JPG', align = "left")
                     ),               
                     column(width =2,
                            fluidRow(       
                              # p(global_cases, style = "font-family: 'times'; font-size:24px;color:black"),
                              p("Cases", style = "font-family: 'times'; font-size:12px;color:black"),
                            )
                     ),
                     column(width =2,
                            img(src='Deaths.JPG', align = "left")              
                     ),               
                     column(width =2,
                            fluidRow(       
                              # p(global_deaths, style = "font-family: 'times'; font-size:24px;color:black"),
                              p("Deaths", style = "font-family: 'times'; font-size:12px;color:black"),
                            ),
                     ),
                     column(width =2,
                            img(src='Recovered.JPG', align = "left")              
                     ),               
                     column(width =2,
                            fluidRow(       
                              # p(global_recovered, style = "font-family: 'times'; font-size:24px;color:black"),
                              p("Recovered", style = "font-family: 'times'; font-size:12px;color:black"),
                            ),
                     )                   
                   ),
            )
        ),
      )
    ),
    
    # Updating input to pull the country data from the server.r
    selectInput("varCounty", 
                label = "Choose a country",
                choices = c("Singapore", 
                            "Hong Kong",
                            "Thailand", 
                            "South Korea",
                            "Japan",
                            "Malaysia",
                            "Thailand",
                            "United States",
                            "Australia"),
                selected = "United States"),
    
    
    
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
        tabPanel("Interactive Map",leafletOutput("map", width = "100%", height =  700)),
        tabPanel("By The Numbers",htmlOutput("filetable"),
                 tableOutput("covid_virus")),
        tabPanel("Trend",
                 img(src='Coronavirus.JPG', align = "left", width = 150),
          textOutput("selected_country"),
          tags$head(tags$style("#selected_country{color: darkorange;
                                     font-size: 40px;
                                     font-family: times;
                                     }"
          )),
        ),  
        tabPanel("Situation Report", 
                  tags$iframe(style="height:700px; width:100%; scrolling=yes", 
                              src=situatation_report_pdf))
        ),
    ),    
  )
)