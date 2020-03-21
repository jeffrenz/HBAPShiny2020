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
library(png)
library(grid)
#library(ggimage)
library(sp)

#for hover
# library(RColorBrewer)
# library(scales)
# library(lattice)



# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)
zipdata <- allzips[sample.int(nrow(allzips), 10000),]

# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
zipdata <- zipdata[order(zipdata$centile),]

# Run functions.r script to load
rel_path_from_root <- "scripts/Functions.r"
source(rel_path_from_root)

#data
#confirmed <- read.csv("data/Confirmed.csv")
#mydata=read.csv("data/bikes.csv")



#library(png) # For writePNG function
function(input, output, session) {
 
  observe({

    # Country Virus Totals
    output$country_cases_total <- renderText({ 
      totals_for_country_df <-get_current_totals_for_country(input$varCounty)
      paste("", comma(totals_for_country_df$TotalConfirmedCases))
    })
    output$country_deaths_total <- renderText({ 
      totals_for_country_df <-get_current_totals_for_country(input$varCounty)
      paste("", comma(totals_for_country_df$TotalDeaths))
    })
    output$country_recovered_total <- renderText({ 
      totals_for_country_df <-get_current_totals_for_country(input$varCounty)
      paste("", comma(totals_for_country_df$TotalRecovered))
    })
    output$selected_country_total <- renderText({ 
      paste("", input$varCounty)
    })
    
    # Render more output
    output$text <- renderText({
      input$title
    })
    
    output$selected_country <- renderText({ 
      paste("", input$varCounty)
    }) 

    output$selected_country_table_header <- renderText({ 
      paste("", input$varCounty)
    }) 
    
    output$selected_display <- renderText({ 
      paste("", input$varDisplay)
    }) 

    output$selected_country_table <- renderText({ 
      paste("", input$varCounty)
    })
        
    selectedData <- reactive({  
      
      current_stats_by_state_province <- get_current_stats_by_state_province()
      current_stats_by_state_province <- current_stats_by_state_province[current_stats_by_state_province$Country_Region==input$varCounty,]
      
      tmp <- HTML(
        htmlTable(current_stats_by_state_province) 
      )
      tmp <- gsub('<td', '<td nowrap="nowrap"; ', tmp)
      tmp <- gsub('<table', '<table style="width:600px"; ', tmp)
      tmp
      
      # Create the table (using table from htmlTables doc as example)
      #g1_name="Confirmed"
      #g2_name="Deaths&dagger;"
      
      # tmp <- HTML(
      #   htmlTable(matrix(paste("Data ", LETTERS[1:16]), 
      #                    ncol=4, byrow = TRUE),
      #             header =  paste(c("Total", "New",
      #                               "Total", "New"), ""),
      #             rnames = paste(c("Mar-13", "Mar-12",
      #                              "Mar-13", "Mar-12"), ""),
      #             rgroup = c("Colorado",
      #                        "Washington"),
      #             n.rgroup = c(2,2),
      #             cgroup = c(g1_name, g2_name),
      #             n.cgroup = c(2,2), 
      #             caption="Source: http://hgis.uw.edu/virus/assets/virus.csv",
      #             tfoot="&dagger; Placeholder") 
      # )
      # tmp <- gsub('<td', '<td nowrap="nowrap"; ', tmp)
      # tmp <- gsub('<table', '<table style="width:600px"; ', tmp)
      # tmp
      
      })
    
    output$filetable <- renderUI({selectedData()})  
    
    # Get Map Data
    map_stats <- get_map_stats()

    pal <- colorFactor(c("red", "orange","yellow"), domain = c( "large", "medium","small"))
    
    ## Interactive map ###########################################
    
    output$map <- renderLeaflet({
      #leaflet(options = leafletOptions(zoomControl = TRUE))
      #leaflet(map_stats) %>% 
      leaflet(map_stats) %>% 
        addTiles(
          urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
          attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'         
        ) %>%
          addCircleMarkers(
          radius = ~ifelse(type == "large", 25, ifelse(type == "medium", 15, 5)),
          color = ~pal(type),
          stroke = FALSE, fillOpacity = 0.5
        ) %>%
        #addLegend("bottomleft", pal=pal, values=c("low (1-100)","medium (100-1000)","high >10000"), title="Severity",
        #          layerId="colorLegend") %>%
        setView(lng = -93.85, lat = 37.45, zoom = 4)
    })
    
    # A reactive expression that returns the set of zips that are
    # in bounds right now
    zipsInBounds <- reactive({
      if (is.null(input$map_bounds))
        return(zipdata[FALSE,])
      bounds <- input$map_bounds
      latRng <- range(bounds$north, bounds$south)
      lngRng <- range(bounds$east, bounds$west)
      subset(zipdata,
             latitude >= latRng[1] & latitude <= latRng[2] &
               longitude >= lngRng[1] & longitude <= lngRng[2])
      
    })    
    

    # This observer is responsible for maintaining the circles and legend,
    # according to the variables the user has chosen to map to color and size.
    
    observe({
      colorBy <- "superzip"
      sizeBy <- "superzip"
      threshold <- 0.05
      
      colorData <- ifelse(zipdata$centile >= (100 - threshold), "yes", "no")
      pal <- colorFactor("viridis", colorData)
      radius <- ifelse(zipdata$centile >= (100 - threshold), 30000, 3000)
     
      # leafletProxy("map", data = zipdata) %>%
      #   clearShapes() %>%
      #   addCircles(~longitude, ~latitude, radius=radius, layerId=~zipcode,
      #              stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) 
    })
    
    # Show a popup at the given location
    showZipcodePopup <- function(zipcode, lat, lng) {
      selectedZip <- allzips[allzips$zipcode == zipcode,]
      content <- as.character(tagList(
        tags$h4("Score:", as.integer(selectedZip$centile)),
        tags$strong(HTML(sprintf("%s, %s %s",
             selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
        ))), tags$br(),
        sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
        sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
        sprintf("Adult population: %s", selectedZip$adultpop)
      ))
      leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
    }
    
    # When map is clicked, show a popup with city info
    observe({
      leafletProxy("map") %>% clearPopups()
      event <- input$map_shape_click
      if (is.null(event))
        return()
      
      isolate({
        showZipcodePopup(event$id, event$lat, event$lng)
      })
    }) 
      

    ## plot In Trend Tab ###########################################
    output$covid_plot_by_country <- renderPlot({
      
      my_image=readJPEG("www/stop_virus.jpeg")
      
      sc_graph <- paste("", input$varDisplay)
      virus_plot_title <- paste0(input$varCounty," Confirmed New Cases Last 14 Days")
      
      # Get Data
      trend_df <- get_trend_data(input$varCounty)
      trend_df$ReportDate <- as.Date(trend_df$ReportDate)
      
      # Plot actual data
      par(new=TRUE)
      p_2 <-ggplot(trend_df, aes(x = factor(ReportDate), y = NewConfirmedCases))+
        annotation_custom(rasterGrob(my_image, 
                                     width = unit(1,"npc"), 
                                     height = unit(1,"npc")), 
                          -Inf, Inf, -Inf, Inf) +
        geom_bar(stat="identity", fill = "#000000", position = "dodge", width = .75, colour = 'black', alpha = 0.1) +
        scale_y_continuous('New Cases', limits = c(0, max(trend_df$NewConfirmedCases) + max(trend_df$NewConfirmedCases)/4)) +
        geom_text(aes(label = round(trend_df$NewConfirmedCases), ymax = 0), size = 6, fontface = 2,
                  colour = 'black', hjust = 0.5, vjust = -1) +
        theme(axis.text.x = element_text(angle=45, hjust = 1)) +
        theme(plot.title = element_text(hjust = 0.5))
      p_2 <- p_2 + ggtitle(virus_plot_title) + xlab("https://data.humdata.org/dataset/novel-coronavirus-2019-ncov-cases") + ylab("COVID-19 New Cases")
      p_2
    }
    )   

    ## Side Bar Vertical Plot ###########################################
    output$covid_plot_by_cases <- renderPlot({
      
      # Get Data
      current_stats_by_country_region_df <- get_current_stats_by_country_region()
      current_stats_by_country_region_df <- head(current_stats_by_country_region_df, 10)
      
      p_cases <- ggplot(current_stats_by_country_region_df, aes(x = reorder(current_stats_by_country_region_df$CountryOrRegion, current_stats_by_country_region_df$TotalConfirmedCases)   , y = current_stats_by_country_region_df$TotalConfirmedCases))+
        geom_bar(stat="identity", fill = "black", position = "dodge", width = .75, colour = 'black', alpha = 0.1) +
        scale_y_continuous('', limits = c(0, max(current_stats_by_country_region_df$TotalConfirmedCases) + max(current_stats_by_country_region_df$TotalConfirmedCases)/2)) +
        geom_text(aes(label = comma(current_stats_by_country_region_df$TotalConfirmedCases), ymax = 0), size = 5, fontface = 2,
                  colour = 'red', hjust = -0.25, vjust = 0.3) 
      
        p_cases <-  p_cases+ theme(plot.title = element_text(face = "bold"),axis.text.x = element_blank(),panel.background = element_rect(fill = "white"),plot.background = element_rect(fill = "white"),panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +labs(x = "", y = "")
      #p_cases+ theme_void()+ coord_flip()
      p_cases + coord_flip()+ ggtitle("Total Confirmed Cases")

    })         
  })
    
}