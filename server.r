#install.packages(c('ggplot2', 'shiny','htmlTable'))
library(shiny)
# library(shinyjs)
library(ggplot2)

library(leaflet)
# library(spData)
library(dplyr)
library(ggmap)

# library(RODBC)
library(stringr)
library(httr)

#library(png) # For writePNG function
function(input, output, session) {
 
  observe({

    output$text <- renderText({
      input$title
    })
    
    output$selected_country <- renderText({ 
      paste("", input$varCounty)
    }) 
    
    output$selected_display <- renderText({ 
      paste("", input$varDisplay)
    }) 
    
    # selectedData <- reactive({  
    #   # Create the table (using table from htmlTables doc as example)
    #   g1_name="Confirmed"
    #   g2_name="Deaths&dagger;"
    #   
    #   tmp <- HTML(
    #     htmlTable(matrix(paste("Data ", LETTERS[1:16]), 
    #                      ncol=4, byrow = TRUE),
    #               header =  paste(c("Total", "New",
    #                                 "Total", "New"), ""),
    #               rnames = paste(c("Mar-13", "Mar-12",
    #                                "Mar-13", "Mar-12"), ""),
    #               rgroup = c("Colorado",
    #                          "Washington"),
    #               n.rgroup = c(2,2),
    #               cgroup = c(g1_name, g2_name),
    #               n.cgroup = c(2,2), 
    #               caption="Source: http://hgis.uw.edu/virus/assets/virus.csv",
    #               tfoot="&dagger; Placeholder") 
    #   )
    #   tmp <- gsub('<td', '<td nowrap="nowrap"; ', tmp)
    #   tmp <- gsub('<table', '<table style="width:600px"; ', tmp)
    #   tmp
    # })
    
    # output$filetable <- renderUI({selectedData()})  
    
    # map code from Ken
    # points <- eventReactive(input$recalc, {
    #   cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
    # }, ignoreNULL = FALSE)
    # 
    # 
    # # Make a list of icons. We'll index into it based on name.
    #  oceanIcons <- iconList(
    #    ship = makeIcon("ferry-18.png", "ferry-18@2x.png", 18, 18),
    #    pirate = makeIcon("danger-24.png", "danger-24@2x.png", 24, 24)
    #  )
    # 
    # # Some fake data
    # df <- sp::SpatialPointsDataFrame(
    #   cbind(
    #     (runif(30) - .5) * 12 - 80.620130,  # lng
    #     (runif(30) - .5) * 4.8 + 24.638077  # lat
    #   ),
    #   data.frame(type = factor(
    #     ifelse(runif(30) > 0.75, "pirate", "ship"),
    #     c("ship", "pirate")
    #     
    #   ))
    # )
    # 
    # # Create a palette that maps factor levels to colors
    # pal <- colorFactor(c("navy", "red"), domain = c("ship", "pirate"))
    
    
    
    # output$map <- renderLeaflet({
    #   # Put three lines of leaflet code here
    #   leaflet(df) %>%
    #     addTiles() %>%
    #     ##addProviderTiles(providers$Stamen.TonerLite,
    #     ##               options = providerTileOptions(noWrap = FALSE)
    #     ##) %>%
    #     # addMarkers()
    #     ## addMarkers(data = points())
    # 
    #     # Select from oceanIcons based on df$type (need "gif")
    #     ## addMarkers(icon = ~oceanIcons[type])
    # 
    #     addCircleMarkers(
    #       radius = ~ifelse(type == "ship", 6, 10),
    #       color = ~pal(type),
    #       stroke = FALSE, fillOpacity = 0.5
    #     )
    # })
    
  })

    
}