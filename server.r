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

# Run functions.r script to load
rel_path_from_root <- "scripts/Functions.r"
source(rel_path_from_root)

#data
#confirmed <- read.csv("data/Confirmed.csv")
#mydata=read.csv("data/bikes.csv")



#library(png) # For writePNG function
function(input, output, session) {
 
  observe({

    # updateSelectInput(session,"select_country_with_updateSelectInput",
    #                   choices = sort(unique(confirmed$Country.Region)))
    
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
    
    output$map <- renderLeaflet({
      # Put three lines of leaflet code here
      #leaflet(df) %>%
      leaflet(options = leafletOptions(zoomControl = TRUE))
      leaflet(map_stats) %>% 
        addTiles() %>%
          addCircleMarkers(
          radius = ~ifelse(type == "large", 25, ifelse(type == "medium", 15, 5)),
          color = ~pal(type),
          stroke = FALSE, fillOpacity = 0.5
        )
    })  

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
    })   

    output$covid_plot_by_cases <- renderPlot({
      
      #my_image=readJPEG("www/stop_virus.jpeg")
      
      #sc_graph <- paste("", input$varDisplay)
      
      
      # Get Data
      current_stats_by_country_region_df <- get_current_stats_by_country_region()
      current_stats_by_country_region_df <- head(current_stats_by_country_region_df, 10)
      
      p_cases <- ggplot(current_stats_by_country_region_df, aes(x = reorder(current_stats_by_country_region_df$CountryOrRegion, current_stats_by_country_region_df$TotalConfirmedCases)   , y = current_stats_by_country_region_df$TotalConfirmedCases))+
        geom_bar(stat="identity", fill = "black", position = "dodge", width = .75, colour = 'black', alpha = 0.1) +
        scale_y_continuous('', limits = c(0, max(current_stats_by_country_region_df$TotalConfirmedCases) + max(current_stats_by_country_region_df$TotalConfirmedCases)/6)) +
        geom_text(aes(label = comma(current_stats_by_country_region_df$TotalConfirmedCases), ymax = 0), size = 5, fontface = 2,
                  colour = 'red', hjust = -0.25, vjust = 0.3) 
      
        p_cases <-  p_cases+ theme(axis.text.x = element_blank(),panel.background = element_rect(fill = "white"),plot.background = element_rect(fill = "white"),panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +labs(x = "", y = "")
      #p_cases+ theme_void()+ coord_flip()
      p_cases + coord_flip()

    })         
  })

    
}