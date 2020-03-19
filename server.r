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

#Get Global Data for Dashboard
g_stats <- get_global_stats()
global_cases <- comma(g_stats$TotalConfirmed)
global_deaths <- comma(g_stats$TotalDeaths)
global_recovered <- comma(g_stats$TotalRecovered)

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
      
      current_stats_by_country <- get_current_stats_by_country()
      current_stats_by_country <- current_stats_by_country[current_stats_by_country$Country_Region==input$varCounty,]
      
      tmp <- HTML(
        htmlTable(current_stats_by_country) 
      )
      tmp <- gsub('<td', '<td nowrap="nowrap"; ', tmp)
      tmp <- gsub('<table', '<table style="width:600px"; ', tmp)
      tmp
      
      
      
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
    #coordinates(map_stats) <- ~Long+Lat

    pal <- colorFactor(c("red", "orange","yellow"), domain = c( "large", "medium","small"))
    
    output$map <- renderLeaflet({
      # Put three lines of leaflet code here
      #leaflet(df) %>%
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
        
  })

    
}