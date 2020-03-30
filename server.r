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
library(sp)

library(shinydashboard)
library(plotly)
library(tidyverse)
library(extrafont)



#font_import()
# This will show more detailed information about fonts
#fonttable()

# Run functions.r script to load
rel_path_from_root <- "scripts/Functions.r"
source(rel_path_from_root)

# Global Stats
g_stats <- get_global_stats()
global_cases <- comma(g_stats$TotalConfirmed)
global_deaths <- comma(g_stats$TotalDeaths)
global_recovered <- comma(g_stats$TotalRecovered)
global_cfr <- g_stats$CFR
# External URLs
url_kinsa <<- "https://healthweather.us/"

#library(png) # For writePNG function
function(input, output, session) {
   
  observe({

    # global Totals
    output$global_header <- renderText({
      "World Wide"
    })
    output$global_cases <- renderText({ 
      global_cases_df <-get_global_stats()
      paste("", comma(global_cases_df$TotalConfirmed))
    })
    output$global_deaths <- renderText({ 
      global_cases_df <-get_global_stats()
      paste("", comma(global_cases_df$TotalDeaths))
    })
    output$global_cfr <- renderText({ 
      global_cases_df <-get_global_stats()
      paste("", global_cases_df$CFR)
    })    
    output$global_recovered <- renderText({ 
      global_cases_df <-get_global_stats()
      paste("", comma(global_cases_df$TotalRecovered))
    })
    
    
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
    output$country_recovered_cfr <- renderText({ 
      totals_for_country_df <-get_current_totals_for_country(input$varCounty)
      paste("", totals_for_country_df$CFR)
    })    
    output$selected_country_total <- renderText({ 
      paste("", input$varCounty)
    })
    
    
    # WHO Situtaion Report Name
    output$situation_report_name <- renderText({ 
      report_name <-get_situatation_report_pdf()
      paste("", report_name)
    })
    
    # Kinsa
    output$kinsa_iframe <- renderUI({
      iframe <- tags$iframe(src=url_kinsa, height=625, width="75%")
      print(iframe)
      iframe
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
      
      current_stats_by_state_province <- get_current_stats_by_state_province(input$varCounty)
      #current_stats_by_state_province <- current_stats_by_state_province[current_stats_by_state_province$Country_Region==input$varCounty,]
      
      tmp <- HTML(
        htmlTable(current_stats_by_state_province) 
      )
      #tmp <- gsub('<td', '<td nowrap="nowrap"; ', tmp)
      tmp <- gsub('<td', '<td ', tmp)
      tmp <- gsub('<table', '<table style="width:800px"; ', tmp)
      tmp
      
      # straightforward column width adjustment
      # calc_cro_cpct(mtcars, list(vs, am), list(total(), vs %nest% am)) %>%
      #   htmlTable(., css.cell = c("width: 250px", # first column width
      #                             rep("width: 50px", ncol(.) - 1)) # other columns width
      #   )
      
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

    pal <- colorFactor(c("#CC0000", "#FF6600","#FF9900"), domain = c( "large", "medium","small"))
    
    ## Interactive map ###########################################
    
    output$map <- renderLeaflet({
      leaflet(map_stats) %>% 
        addTiles(
          urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
          attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'         
        ) %>%
          addCircleMarkers(
          radius = ~ifelse(type == "large", 25, ifelse(type == "medium", 15, 6)),
          color = ~pal(type),
          stroke = FALSE, fillOpacity = 0.5,
          popup = ~paste(GeographyKey,"<br/><br/>","Confirmed Cases: ", comma(ConfirmedCases), "<br/>", "Deaths: ", comma(Deaths), "<br/>", "CFR: ", CFR)
          
        ) %>%
        setView(lng = -93.85, lat = 37.45, zoom = 5) %>%
        #setView(lng = -33.85, lat = 27.45, zoom = 3) %>%
        addLegend("bottomleft", 
                                colors =c("#FF9900","#FF6600","#CC0000"),
                                labels= c("0-100", "101-1,000","1,000+"),
                                title= "Confirmed Cases",
                                opacity = 1)
    })
    

    output$over_nutrient <- renderValueBox({
      # nutrition_df <- dv_df() %>% 
      #   # filter(NutrientID %in% c(601, 204, 307, 269, 0)) %>% 
      #   tidyr::drop_na(pct_dv) %>% filter(pct_dv > 100)
      # if(nrow(nutrition_df) > 0){
      #   valueBox("Over Daily Value", HTML(paste0(nutrition_df$Nutrient, sep="<br>")), icon = icon("exclamation-triangle"), color = "red")
      # } else {
      #   valueBox("All nutrients", "below recommended DV", icon = icon("exclamation-triangle"), color = "green")
      # }
      valueBox("All nutrients", "below recommended DV", icon = icon("exclamation-triangle"), color = "green", width=5)
    })
      

    ## plot In Trend Tab ###########################################
      output$covid_plot_by_country <- renderPlot({
      #output$covid_plot_by_country <- renderPlotly({   
      my_image=readJPEG("www/CoronavirusChartBackground.jpg")
      #my_image=readJPEG("www/doctors.jpg")
      sc_graph <- paste("", input$varDisplay)
      virus_plot_title <- paste0("New Cases Last 14 Days: ",input$varCounty)
      
      # Get Data
      pDaysAgo <-14
      trend_df <- get_trend_data(input$varCounty)
      trend_df$ReportDate <- as.Date(trend_df$ReportDate)
      
      # Create Linear Model
      predlm = lm(NewConfirmedCases ~ DaysAgo, data = trend_df)
      predslm = predict(predlm, interval = "confidence")
      
      # Make Predictions
      #pred_today<-predict(predlm, data.frame(DaysAgo = c(0)), interval = "confidence")
      #pred_tomorrow<-predict(predlm, data.frame(DaysAgo = c(-1)), interval = "confidence")
      #predslm = rbind(predslm,pred_today)
      #predslm = rbind(predslm,pred_tomorrow)

      # Add rows for today and tomorrow
      #new_values_today <- c("US",as.character(Sys.Date()),0,NA,NA,NA,NA,NA,NA)
      #new_values_tomorrow <- c("US",as.character(Sys.Date()+1),-1,NA,NA,NA,NA,NA,NA)
      #trend_df = rbind(trend_df,new_values_today)
      #trend_df = rbind(trend_df,new_values_tomorrow)
      
      # Add predictions 
      trend_df = cbind(trend_df, predslm)
      
      # Plot actual data
      par(new=TRUE)
      #p_2 <-ggplot(trend_df, aes(x = factor(ReportDate), y = NewConfirmedCases))+
      p_2 <-ggplot(trend_df, aes(x = ReportDate, y = NewConfirmedCases))+
        annotation_custom(rasterGrob(my_image, 
                                     width = unit(1,"npc"), 
                                     height = unit(1,"npc")), 
                          -Inf, Inf, -Inf, Inf) +
        geom_point(aes(y = fit),color="darkorange", size=3)+
        geom_ribbon( aes(ymin = lwr, ymax = upr,fill ="Linear"), alpha = .25,linetype = 2) +
        geom_bar(stat="identity", fill = "white", position = "dodge", width = .75, color = 'white', alpha = 0.25) +
        scale_y_continuous('# of New Cases', limits = c(0, max(trend_df$NewConfirmedCases) + max(trend_df$NewConfirmedCases)/2)) +
        geom_text(aes(label = comma(NewConfirmedCases)), size = 6, fontface = 2,
                  color = 'white', hjust = 0.5, vjust = -0.5) +
        theme(axis.text.x = element_text(angle=45, hjust = 1)) +
        theme(plot.title = element_text(hjust = 0.0, size = 28, color = 'darkorange', face = "bold",family="Bradley Hand ITC")) +
        theme(plot.caption = element_text(vjust = -1, hjust = 0)) +
        theme(axis.title.x = element_text(face = "bold", vjust=-1)) +
        theme(axis.title.y = element_text(face = "bold")) +
        theme(text=element_text(size=16)) +
        scale_fill_manual(values=c("darkorange","white"), name="fill") 
        #scale_fill_manual(values=c("black"), name="color")
      
      p_2 <- p_2 + labs(title  = virus_plot_title,
                           caption = "Source: https://github.com/CSSEGISandData/COVID-19",
                           ylab = paste0("Total Cases (",input$varDisplay,")")
                        ) 
      p_2 <- p_2 + xlab("Report Date") + ylab("COVID-19 New Cases")
      
      # Format : month/year
      p_2 <- p_2 + scale_x_date(date_breaks = "1 day",date_labels = "%B %d")
      
      # Using a manual scale instead of hue
      p_2 <- p_2 + guides(fill=guide_legend(title="Prediction"))
      p_2
    }
    )   

    ## Side Bar Vertical Plot ###########################################
    output$covid_plot_by_cases <- renderPlot({
      
      # Get Data
      current_stats_by_country_region_df <- get_current_stats_by_country_region()
      current_stats_by_country_region_df <- head(current_stats_by_country_region_df, 10)
      
      p_cases <- ggplot(current_stats_by_country_region_df, aes(x = reorder(CountryOrRegion, TotalConfirmedCases)   , y = TotalConfirmedCases))+
        geom_bar(stat="identity", fill = "black", position = "dodge", width = .75, colour = 'black', alpha = 0.1) +
        scale_y_continuous('', limits = c(0, max(current_stats_by_country_region_df$TotalConfirmedCases) + max(current_stats_by_country_region_df$TotalConfirmedCases)/2)) +
        geom_text(aes(label = comma(TotalConfirmedCases), ymax = 0), size = 5, fontface = 2,
                  colour = 'red', hjust = -0.2, vjust = 0.3) 
      
        p_cases <-  p_cases+ theme(plot.title = element_text(face = "bold"),axis.text.x = element_blank(),panel.background = element_rect(fill = "white"),plot.background = element_rect(fill = "white"),panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +labs(x = "", y = "")
      #p_cases+ theme_void()+ coord_flip()
        p_cases <-p_cases + coord_flip()+ ggtitle("Total Confirmed Cases")
        p_cases

    })         
  })
    
}