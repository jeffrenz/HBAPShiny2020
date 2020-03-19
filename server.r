#install.packages(c('ggplot2', 'shiny','htmlTable'))
library(shiny)
library(shinyjs)
library(ggplot2)

library(leaflet)
library(spData)
library(dplyr)
library(ggmap)

library(RODBC)
library(odbc)

library(stringr)
library(httr)
library(jpeg)

library(sp)
# Run functions.r script to load
rel_path_from_root <- "scripts/Functions.r"
source(rel_path_from_root)

#data
confirmed <- read.csv("data/Confirmed.csv")
mydata=read.csv("data/bikes.csv")

#Get Global Data for Dashboard
g_stats <- get_global_stats()
global_cases <- comma(g_stats$TotalConfirmed)
global_deaths <- comma(g_stats$TotalDeaths)
global_recovered <- comma(g_stats$TotalRecovered)

# Get Data
map_stats <- get_map_stats()
coordinates(map_stats) <- ~Long+Lat

#library(png) # For writePNG function
function(input, output, session) {
 
  observe({

    updateSelectInput(session,"select_country_with_updateSelectInput",
                      choices = sort(unique(confirmed$Country.Region)))
    
    output$text <- renderText({
      input$title
    })
    
    output$selected_country <- renderText({ 
      paste("", input$varCounty)
    }) 
    
    output$selected_display <- renderText({ 
      paste("", input$varDisplay)
    }) 
    
    # output$selected_country_graph <- renderText({ 
    #   #mydata=read.csv("data/bikes.csv")
    #   confirmed <- read.csv("data/Confirmed.csv")
    #   paste("", input$varCounty)
    # })     

    output$selected_country_table <- renderText({ 
      paste("", input$varCounty)
    })
        
    selectedData <- reactive({  
      # Create the table (using table from htmlTables doc as example)
      g1_name="Confirmed"
      g2_name="Deaths&dagger;"
      
      tmp <- HTML(
        htmlTable(matrix(paste("Data ", LETTERS[1:16]), 
                         ncol=4, byrow = TRUE),
                  header =  paste(c("Total", "New",
                                    "Total", "New"), ""),
                  rnames = paste(c("Mar-13", "Mar-12",
                                   "Mar-13", "Mar-12"), ""),
                  rgroup = c("Colorado",
                             "Washington"),
                  n.rgroup = c(2,2),
                  cgroup = c(g1_name, g2_name),
                  n.cgroup = c(2,2), 
                  caption="Source: http://hgis.uw.edu/virus/assets/virus.csv",
                  tfoot="&dagger; Placeholder") 
      )
      tmp <- gsub('<td', '<td nowrap="nowrap"; ', tmp)
      tmp <- gsub('<table', '<table style="width:600px"; ', tmp)
      tmp
    })
    
    output$filetable <- renderUI({selectedData()})  
    
    # map code from Ken
    points <- eventReactive(input$recalc, {
      cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
    }, ignoreNULL = FALSE)
    
    
    # Make a list of icons. We'll index into it based on name.
     oceanIcons <- iconList(
       ship = makeIcon("ferry-18.png", "ferry-18@2x.png", 18, 18),
       pirate = makeIcon("danger-24.png", "danger-24@2x.png", 24, 24)
     )
    
    # Some fake data
    df <- sp::SpatialPointsDataFrame(
      cbind(
        (runif(30) - .5) * 12 - 80.620130,  # lng
        (runif(30) - .5) * 4.8 + 24.638077  # lat
      ),
      data.frame(type = factor(
        ifelse(runif(30) > 0.75, "pirate", "ship"),
        c("ship", "pirate")
        
      ))
    )
    
    # Create a palette that maps factor levels to colors
    # pal <- colorFactor(c("navy", "red"), domain = c("ship", "pirate"))
    pal <- colorFactor(c("green", "navy", "red"), domain = c("small", "medium", "large"))
    
    output$map <- renderLeaflet({
      # Put three lines of leaflet code here
      #leaflet(df) %>%
      leaflet(map_stats) %>% 
        addTiles() %>%
      
          addCircleMarkers(
          ##radius = ~ifelse(type == "ship", 6, 10),
          color = ~pal(type),
          stroke = FALSE, fillOpacity = 0.5
        )
    })  

    output$covid_plot_by_country <- renderPlot({
      # if(input$update_chart == 0){
      #   return()
      # }
      my_image=readJPEG("www/stop_virus.jpeg")
      #my_image=readJPEG("www/StaySafe.jpg")
      
      sc_graph <- paste("", input$varDisplay)
      virus_plot_title <- paste0(input$varCounty," Confirmed New Cases Last 14 Days")
      
      # Set up a plot with correct labels and dimensions and apply background image 
      hist(mydata$Count, main=virus_plot_title, xlab = "Date", ylab = "Number of Cases")
      lim <- par()
      rasterImage(my_image, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
      
      # Plot actual data
      par(new=TRUE)
      hist(mydata$Count,col=rgb(0.1,0.1,0.1,0.15), main=virus_plot_title, xlab = "Date", ylab = "Number of Cases")
    })    
    
    output$covid_with_updateSelectInput <- renderPlot({
      if(input$update_chart == 0){
        return()
      }
      
      confirmed %>%
        filter(
          Country.Region == isolate(input$varCounty)
        ) %>%
        ggplot(aes(x=Value))+
        geom_histogram(binwidth = 40)+
        labs(
          title = paste("Confirmed cases by days in the last 3 months", "in",
                        isolate(input$select_country_with_updateSelectInput)),
          subtitle = "Data source: https://data.humdata.org/dataset/novel-coronavirus-2019-ncov-cases"
        )
    })    
    
    
        
  })

    
}