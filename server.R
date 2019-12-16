library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(geojsonio)
library(ggplot2)

#load Malaysia geojson
states <- geojsonio::geojson_read("data/my-states.geojson", what = "sp")
#Load data
rainfall <- read.csv("data/rainfall.csv")
#Rename column for Rainfall(mm)
rainfall <- rainfall %>% dplyr::rename(Rainfall = 5)
#Rename State value
levels(rainfall$State)[levels(rainfall$State)=="Selangor-Wilayah"] <- "Federal Territory of Kuala Lumpur"
levels(rainfall$State)[levels(rainfall$State)=="NSembilan"] <- "Negeri Sembilan"
#Create single column for Date
rainfall$Date <- as.Date(with(rainfall, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")
#Convert column to factor
rainfall$Year <- factor(rainfall$Year)
rainfall$Month <- factor(rainfall$Month)
rainfall$Day <- factor(rainfall$Day)

#Filter & calculate rainfall average function
calculate_rainfall_mean <- function(yearfilter = rainfall$Year, monthfilter = rainfall$Month, statefilter = rainfall$State){
    
    if(yearfilter == "All Years")
        yearfilter = rainfall$Year
    if(monthfilter == "All Months")
        monthfilter = rainfall$Month
    
    rainfallfilter <- rainfall %>% filter(Year == yearfilter, Month == monthfilter, State == statefilter)
    mean(rainfallfilter$Rainfall)
}

#Tabulate list of state & average rainfall for leaflet map coloring
rainfall_map <- as.data.frame(table(rainfall$State)) %>% dplyr::rename(State = 1) %>% select(State)

for( i in rownames(rainfall_map) )
    rainfall_map[i, "Rainfall"] <- calculate_rainfall_mean(statefilter = rainfall_map[i, "State"])

#Coloring map
bins <- c(0, 2.5, 7.6, 50, Inf)
pal <- colorBin("YlOrRd", domain = rainfall_map$Rainfall, bins = bins)

#Coloring barplot condition
pal1 <- c("Light" = "#ffffcc", "Moderate" = "#fed976", "Heavy" = "#feb24c", "Violent" = "#e31a1c")
pal2 <- c("Fine" = "green", "Bad" = "red")

function(input, output, session) {
    
    #Pop up tutorial on start up
    showModal(
        modalDialog(
            title = "How to use this app",
            HTML("1. Use the left filter tab to visualize rainfall intensity in map.<br> 
                    2. Use the below filter tab to analyze rainfall in detail."),
            easyClose = TRUE,
            footer = tagList(
                modalButton("Got It!")
            )
        )
    )
    
    #Change dropdown to current date
    observeEvent(input$Today, {
            currentTime <- Sys.time()
            as.POSIXct(format(currentTime),tz="Asia/Kuala_Lumpur")
        
            updateSelectInput(session, inputId = "Year", selected = format(currentTime, "%Y"))
            updateSelectInput(session, inputId = "Month", selected = format(currentTime, "%m"))
            updateSelectInput(session, inputId = "Day", selected = format(currentTime, "%d"))
            
            updateSelectInput(session, inputId = "YearAnalyze", selected = format(currentTime, "%Y"))
            updateSelectInput(session, inputId = "MonthAnalyze", selected = format(currentTime, "%m"))
    })
    
    observe({

        if(input$Day != "All Days")
            rainfall_map <- rainfall %>% filter(Year == input$Year, Month == input$Month, Day == input$Day)
        else
            for( i in rownames(rainfall_map) )
                rainfall_map[i, "Rainfall"] <- calculate_rainfall_mean(input$Year, input$Month, rainfall_map[i, "State"])
            
        for( i in rownames(rainfall_map) ){
          if(rainfall_map[i, "Rainfall"] <2.5)
            rainfall_map[i, "Intensity"] <- "Light"
          else if(rainfall_map[i, "Rainfall"] <7.6)
            rainfall_map[i, "Intensity"] <- "Moderate"
          else if(rainfall_map[i, "Rainfall"] <50)
            rainfall_map[i, "Intensity"] <- "Heavy"
          else
            rainfall_map[i, "Intensity"] <- "Violent"
        }
            
            
            output$map <- renderLeaflet({
                leaflet(states, options = leafletOptions(zoomControl = FALSE)) %>%
                    setView(lng = 102.5000, lat = 4.0000, zoom = 6) %>%
                    addTiles(
                        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
                    ) %>%
                    #Map layer customization
                    addTiles(group = "OSM (default)") %>%
                    addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
                    addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
                    addLayersControl(
                        baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
                        options = layersControlOptions(collapsed = TRUE, position = "bottomleft")
                    ) %>%
                    #End - Map layer customization
                    addPolygons(
                        fillColor = ~pal(rainfall_map$Rainfall),
                        weight = 2,
                        opacity = 1,
                        color = "black",
                        dashArray = "3",
                        fillOpacity = 0.8,
                        highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.7,
                            bringToFront = TRUE),
                        label = sprintf("<strong>%s</strong>", states$name) %>% lapply(htmltools::HTML),
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "15px",
                            direction = "auto")
                    ) %>%
                    leaflet::addLegend("bottomright", 
                                       colors =c("#ffffcc", "#fed976", "#feb24c", "#e31a1c"),
                                       labels= c("Light", "Moderate","Heavy","Violent"),
                                       title= "Classification",
                                       opacity = 1)
            })
            
            output$overallTrend <- renderPlot({
                
                #Rename lengthy State
                levels(rainfall_map$State)[levels(rainfall_map$State)=="Federal Territory of Kuala Lumpur"] <- "KL"
                levels(rainfall_map$State)[levels(rainfall_map$State)=="Negeri Sembilan"] <- "N9"
  
                # Render a bar plot
                ggplot(rainfall_map, aes(x = State, y = Rainfall))+
                    geom_col(aes(fill = Intensity) , show.legend = FALSE)+
                    labs(title="Trend of Rainfall", y="Average Rainfall (mm)", x="State")+ 
                    scale_fill_manual(values = pal1)+
                    coord_flip()
                
            })
            
    })
    
    
    observe({
        
        output$stateTrend <- renderPlot({
            
            stateTrend <- rainfall %>% filter(State == input$StateAnalyze)
            
            #Render a bar plot
            ggplot(stateTrend, aes(x = Year, y = Rainfall))+
            geom_col(aes(fill = Year), show.legend = FALSE)+
            labs(title="Trend of Rainfall", y="Total Rainfall (mm)", x="Year")
            
        })
        
        output$stateYearlyTrend <- renderPlot({
            
            stateYearlyTrend <- rainfall %>% filter(State == input$StateAnalyze & Year == input$YearAnalyze)
            
            #Render a line plot
            ggplot(stateYearlyTrend, aes(x = Date, y = Rainfall))+
                geom_line(color = "purple")+
                labs(title="Trend of Rainfall", y="Total Rainfall (mm)", x="Month")
            
        })
        
        output$stateMonthlyTrend <- renderPlot({
            
            stateMonthlyTrend <- rainfall %>% filter(State == input$StateAnalyze & Year == input$YearAnalyze & Month == input$MonthAnalyze)
            
            #Classify weather
            stateMonthlyTrend <- mutate(stateMonthlyTrend, Weather = ifelse(Rainfall<6, "Fine", "Bad"))
            
            #Render a bar plot
            ggplot(stateMonthlyTrend, aes(x = Day, y = Rainfall))+
                geom_col(aes(fill = Weather), show.legend = TRUE)+
                labs(title="Trend of Rainfall", y="Total Rainfall (mm)", x="Day")+
                scale_fill_manual(values = pal2)+ 
                theme(legend.position="right")
            
        })
        
    })

}