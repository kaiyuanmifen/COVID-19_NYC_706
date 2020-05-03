# load dependencies:
library(shiny)
library(rgdal)
library(geojsonio)
library(ggplot2)
library(leaflet)
library(plotly)
library(RColorBrewer)
library(dplyr)
library(broom)
library(mapproj)
library(reshape2)
library(digest)
library(lubridate)
library(spdplyr)
library(rmapshaper)
library(raster)
library(mapview)
library(sf)
library(httr)
library(janitor)
library(lattice)
library(sp)
library(ggplot2)
# load datasets:
ny_spdf = geojson_read("../cleaned_data/nyc-zip-code-tabulation-areas-polygons.geojson",  what = "sp")
df_leaflet = ny_spdf
df_final = read.csv("../cleaned_data/non_polygon_df_final.csv", check.names = F, stringsAsFactors = F)
df_final <- clean_names(df_final)

# rename ZIP column to postalCode to enable merge
colnames(df_final)[which(names(df_final) == "zip")] <- "postalCode"
df_final$postalCode <- as.character(df_final$postalCode)

# recode DATE column as date data type
df_final$date <- as.Date(df_final$date, "%Y-%m-%d")
df_leaflet@data = left_join(df_leaflet@data, df_final,by = "postalCode")

df_leaflet$white_prop
df_leaflet$black_prop
plot(df_leaflet$positive_per_capita~df_leaflet$hispanic_prop)
df_leaflet$positive


# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel(NULL, windowTitle = "COVID-19 outbreak in MNYC"),
    
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href="https://fonts.googleapis.com/css?family=Roboto&display=swap")
    ),
    
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href="https://fonts.googleapis.com/css2?family=Lato:wght@300;400&display=swap")
    ),
    
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "myTheme.css")
    ),
    
    fluidRow(
        column(10, offset = 1, align="left",   
               h2("Spatiotemporal evolution of the outbreak"),
               HTML("<p>The following map shows the break down of the NYC COVID-19 cases by zipcodes.
                                 It is possible to use the slider to have a look at the situation on a specific date. 
                                 You can also click on 'positive' for numbers who tested positive for COVID-19, and 'total' for total numbers who were tested.
                                 Finally, by clicking on the play button it is possible to have a look at the spatiotemporal evolution of the epidemic. 
                                 </p>"),
               br()
        )),
 
    fluidRow(
        column(10,offset = 1, align="center",
               selectInput("choose_stat","Statistic of interest",choices = c("positive cases", "positive cases per million capita", "total tested", "total tested per million capita")),
               br(),
        )),
    
    fluidRow(
        column(10,offset = 1, align="left",
               leafletOutput("leaflet_chloropleth"),
               br(),
        )),
    
    fluidRow(
        column(10, offset = 1, align="center",
               sliderInput("date",
                           "Dates:",
                           min = unique(min(as.Date(df_final$date,"%Y-%m-%d"))),
                           max = unique(max(as.Date(df_final$date,"%Y-%m-%d"))),
                           value = unique(max(as.Date(df_final$date,"%Y-%m-%d"))),
                           width="60%", 
                           animate = animationOptions(interval = 5000, loop = FALSE, playButton = NULL,
                                                      pauseButton = NULL)
               ),
               br()
        )),

    fluidRow(
        column(10, offset=1, align="left",
               HTML('Data: <a href="https://www1.nyc.gov/site/doh/covid/covid-19-data.page">NYC Open Data</a>.'),
               br(),
               br(),
               br(),
        )),
    
    #Trend
    plotOutput("Trend"),
    
    #select variable 2 for statistics  
    fluidRow(column(10,offset = 1, align="center",selectInput("Variable2", "Variable2",c("positive","positive_per_capita","tests_per_capita" )),br(),
        )),
    #select variable 1 for statistics 
    fluidRow(column(10,offset = 1, align="center",selectInput("Variable1", "Variable1",c("median_household_income",
                                           "male_prop","female_prop","white_prop",
                                           "black_prop", "asian_prop","hawaiian_pi_prop",
                                           "multi_race_prop","other_prop","hispanic_prop" )),br(),
    )),
    
    #positive rate
    plotOutput("TwoVariablePlot")
)

# Define server logic required to draw a histogram
server = function(input, output, session) {
 
    observe({
        stat=input$choose_stat
        if(stat == 'positive cases'){df_final$Trend=df_final[,"positive"]}
        if(stat == "positive cases per million capita"){df_final$Trend=df_final[,"positive_per_capita"]}
        if(stat == 'total tested'){df_final$Trend=df_final[,"total"]}    
        if(stat == "total tested per million capita"){df_final$Trend=df_final[,"tests_per_capita"]}
 
        
        output$Trend <- renderPlot({
            ggplot(df_final, aes(x=date,y=Trend,color=postalCode)) + 
                geom_line(linetype = "dashed")+
                geom_point()+xlab("date")+ylab(stat)+ theme(legend.position = "none")+
                ggtitle("Trend over time by zip code")
           
        })
    })
    
    
    #Boxplot
    observe({
    df_final$Variable2=df_final[,input$Variable2]
    df_final$Variable1=df_final[,input$Variable1]
    
    output$TwoVariablePlot <- renderPlot({
        ggplot(df_final, aes(x=as.factor(Variable1),y=Variable2)) + 
            geom_boxplot(notch=TRUE)+xlab(input$Variable1)+ylab(input$Variable2)+ theme_bw()+
            ggtitle("County(borough) level statistics")
        #plot(df_final$positive_per_capita~df_final$median_household_income,main="PositiveRate vs. median income",
             #ylab="Positive per capita")
    })
    })
   
    
    
    
    output$leaflet_chloropleth = renderLeaflet({
        
        # encode inputs of user
        stat <- input$choose_stat
        target_date <- as.Date(input$date)
        
        if(stat == 'positive cases'){
            # subset dataframe for inputs
            target_date_pos <- subset(df_leaflet@data, date==target_date, select=c('postalCode','positive'))
            
            mybins = seq(0, 3000, by = 500)
            mypalette = colorBin( palette="YlOrRd", domain = target_date_pos$positive, na.color="transparent", bins=mybins)
            
            mytag = paste(
                "Zipcode: ", df_leaflet@data$postalCode,"<br/>", 
                "Population: ", df_leaflet@data$population,"<br/>", 
                "Positive cases: ",target_date_pos$positive, "<br/>", 
                "Female proportion: ", df_leaflet@data$female_prop,"<br/>",
                "Black proportion: ", df_leaflet@data$black_prop,"<br/>",
                "Asian proportion: ", df_leaflet@data$asian_prop,"<br/>",
                "Hispanic proportion: ", df_leaflet@data$hispanic_prop,"<br/>",
                "White proportion: ", df_leaflet@data$white_prop,"<br/>",
                "Hawaiian/Pasifika proportion: ", df_leaflet@data$hawaiian_pi_prop,"<br/>",
                sep="") %>%
                lapply(htmltools::HTML)
            
            leaflet(df_leaflet) %>% 
                addProviderTiles(providers$CartoDB.Positron) %>%  #add simpler basemap
                #  clearShapes() %>%
                setView(-73.935242, 40.710310, zoom = 9.5) %>%
                addPolygons(data =df_leaflet, 
                            fillColor = ~mypalette(target_date_pos$positive),
                            fillOpacity = 0.5,
                            stroke=TRUE,
                            weight=0.8,
                            color="#000000",
                            label = mytag,
                            labelOptions = labelOptions(
                                style = list("font-weight" = "normal", padding = "3px 8px"),
                                textsize = "10px",
                                direction = "auto"),
                            highlight = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) %>%
                
                clearControls() %>%
                addLegend( pal=mypalette, values=target_date_pos$positive, opacity=0.5, title = "numbers", position = "bottomleft" )
            
        }else if(stat == 'total tested'){
            # subset dataframe for inputs
            target_date_pos <- subset(df_leaflet@data, date==target_date, select=c('postalCode','total'))
            
            mybins = seq(0, 5000, by = 1000)
            mypalette = colorBin( palette="Blues", domain = target_date_pos$total, na.color="transparent", bins=mybins)
            
            mytag = paste(
                "Zipcode: ", df_leaflet@data$postalCode,"<br/>", 
                "Population: ", df_leaflet@data$population,"<br/>",
                "Total tested: ",target_date_pos$total, "<br/>", 
                "Female proportion: ", df_leaflet@data$female_prop,"<br/>",
                "Black proportion: ", df_leaflet@data$black_prop,"<br/>",
                "Asian proportion: ", df_leaflet@data$asian_prop,"<br/>",
                "Hispanic proportion: ", df_leaflet@data$hispanic_prop,"<br/>",
                "White proportion: ", df_leaflet@data$white_prop,"<br/>",
                "Hawaiian/Pasifika proportion: ", df_leaflet@data$hawaiian_pi_prop,"<br/>",
                sep="") %>%
                lapply(htmltools::HTML)
            
            leaflet(df_leaflet) %>% 
                addProviderTiles(providers$CartoDB.Positron) %>%  #add simpler basemap
                #  clearShapes() %>%
                setView(-73.935242, 40.710310, zoom = 9.5) %>%
                addPolygons(data =df_leaflet, 
                            fillColor = ~mypalette(target_date_pos$total),
                            fillOpacity = 0.5,
                            stroke=TRUE,
                            weight=0.8,
                            color="#000000",
                            label = mytag,
                            labelOptions = labelOptions(
                                style = list("font-weight" = "normal", padding = "3px 8px"),
                                textsize = "10px",
                                direction = "auto"),
                            highlight = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) %>%
                clearControls() %>%
                addLegend( pal=mypalette, values=target_date_pos$total, opacity=0.5, title = "Cases", position = "bottomleft" )
        }else if(stat == 'total tested per million capita'){
            # subset dataframe for inputs
            target_date_pos <- subset(df_leaflet@data, date==target_date, select=c('postalCode','tests_per_capita'))
            
            mybins = seq(0, 65000, by = 5000)
            mypalette = colorBin( palette="Blues", domain = target_date_pos$tests_per_capita, na.color="transparent", bins=mybins)
            
            mytag = paste(
                "Zipcode: ", df_leaflet@data$postalCode,"<br/>", 
                "Population: ", df_leaflet@data$population,"<br/>",
                "Total tested per million capita: ",target_date_pos$tests_per_capita, "<br/>",
                "Female proportion: ", df_leaflet@data$female_prop,"<br/>",
                "Black proportion: ", df_leaflet@data$black_prop,"<br/>",
                "Asian proportion: ", df_leaflet@data$asian_prop,"<br/>",
                "Hispanic proportion: ", df_leaflet@data$hispanic_prop,"<br/>",
                "White proportion: ", df_leaflet@data$white_prop,"<br/>",
                "Hawaiian/Pasifika proportion: ", df_leaflet@data$hawaiian_pi_prop,"<br/>",
                sep="") %>%
                lapply(htmltools::HTML)
            
            leaflet(df_leaflet) %>% 
                addProviderTiles(providers$CartoDB.Positron) %>%  #add simpler basemap
                #  clearShapes() %>%
                setView(-73.935242, 40.710310, zoom = 9.5) %>%
                addPolygons(data =df_leaflet, 
                            fillColor = ~mypalette(target_date_pos$tests_per_capita),
                            fillOpacity = 0.5,
                            stroke=TRUE,
                            weight=0.8,
                            color="#000000",
                            label = mytag,
                            labelOptions = labelOptions(
                                style = list("font-weight" = "normal", padding = "3px 8px"),
                                textsize = "10px",
                                direction = "auto"),
                            highlight = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) %>%
                clearControls() %>%
                addLegend( pal=mypalette, values=target_date_pos$tests_per_capita, opacity=0.5, title = "Tests per million capita", position = "bottomleft" )
        }else if(stat == 'positive cases per million capita'){
            # subset dataframe for inputs
            target_date_pos <- subset(df_leaflet@data, date==target_date, select=c('postalCode','positive_per_capita'))
            
            mybins = seq(0, 35000, by = 5000)
            mypalette = colorBin( palette="YlOrRd", domain = target_date_pos$positive_per_capita, na.color="transparent", bins=mybins)
            
            mytag = paste(
                "Zipcode: ", df_leaflet@data$postalCode,"<br/>", 
                "Population: ", df_leaflet@data$population,"<br/>",
                "Positive cases per million capita: ",target_date_pos$positive_per_capita, "<br/>", 
                "Female proportion: ", df_leaflet@data$female_prop,"<br/>",
                "Black proportion: ", df_leaflet@data$black_prop,"<br/>",
                "Asian proportion: ", df_leaflet@data$asian_prop,"<br/>",
                "Hispanic proportion: ", df_leaflet@data$hispanic_prop,"<br/>",
                "White proportion: ", df_leaflet@data$white_prop,"<br/>",
                "Hawaiian/Pasifika proportion: ", df_leaflet@data$hawaiian_pi_prop,"<br/>",
                sep="") %>%
                lapply(htmltools::HTML)
            
            leaflet(df_leaflet) %>% 
                addProviderTiles(providers$CartoDB.Positron) %>%  #add simpler basemap
                #  clearShapes() %>%
                setView(-73.935242, 40.710310, zoom = 9.5) %>%
                addPolygons(data =df_leaflet, 
                            fillColor = ~mypalette(target_date_pos$positive_per_capita),
                            fillOpacity = 0.5,
                            stroke=TRUE,
                            weight=0.8,
                            color="#000000",
                            label = mytag,
                            labelOptions = labelOptions(
                                style = list("font-weight" = "normal", padding = "3px 8px"),
                                textsize = "10px",
                                direction = "auto"),
                            highlight = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) %>%
                clearControls() %>%
                addLegend( pal=mypalette, values=target_date_pos$positive_per_capita, opacity=0.5, title = "Cases per million capita", position = "bottomleft" )
        }
    })
    
    observe({
        
        # note how this mirrors the code above except for leafletProxy (see the if...else... below)
        stat <- input$choose_stat
        
        target_date <- as.Date(input$date)
        
        if(stat == 'positive cases'){
            target_date_pos <- subset(df_leaflet@data, date==target_date, select=c('postalCode','positive'))
            
            mybins = seq(0, 3000, by = 500)
            mypalette = colorBin( palette="YlOrRd", domain = target_date_pos$positive, na.color="transparent", bins=mybins)
            
            mytag = paste(
                "Zipcode: ", df_leaflet@data$postalCode,"<br/>",
                "Population: ", df_leaflet@data$population,"<br/>",
                "Positive cases: ",target_date_pos$positive, "<br/>", 
                "Female proportion: ", df_leaflet@data$female_prop,"<br/>",
                "Black proportion: ", df_leaflet@data$black_prop,"<br/>",
                "Asian proportion: ", df_leaflet@data$asian_prop,"<br/>",
                "Hispanic proportion: ", df_leaflet@data$hispanic_prop,"<br/>",
                "White proportion: ", df_leaflet@data$white_prop,"<br/>",
                "Hawaiian/Pasifika proportion: ", df_leaflet@data$hawaiian_pi_prop,"<br/>",
                sep="") %>%
                lapply(htmltools::HTML)
            
            leafletProxy("leaflet_chloropleth") %>%
                clearShapes() %>%
                setView(-73.935242, 40.710310, zoom = 9.5) %>%
                #addProviderTiles(providers$CartoDB.Positron) %>%  #add simpler basemap
                #  clearShapes() %>%
                addPolygons(data =df_leaflet, 
                            fillColor = ~mypalette(target_date_pos$positive),
                            fillOpacity = 0.5,
                            stroke=TRUE,
                            weight=0.8,
                            color="#000000",
                            label = mytag,
                            labelOptions = labelOptions(
                                style = list("font-weight" = "normal", padding = "3px 8px"),
                                textsize = "10px",
                                direction = "auto"),
                            highlight = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) %>%
                clearControls() %>%
                addLegend( pal=mypalette, values=target_date_pos$positive, opacity=0.5, title = "Cases", position = "bottomleft" )
        }else if(stat == 'total tested'){
            target_date_pos <- subset(df_leaflet@data, date==target_date, select=c('postalCode','total'))
            
            mybins = seq(0, 5000, by = 1000)
            mypalette = colorBin( palette="Blues", domain = target_date_pos$total, na.color="transparent", bins=mybins)
            
            mytag = paste(
                "Zipcode: ", df_leaflet@data$postalCode,"<br/>", 
                "Population: ", df_leaflet@data$population,"<br/>",
                "Total tested: ",target_date_pos$total, "<br/>", 
                "Female proportion: ", df_leaflet@data$female_prop,"<br/>",
                "Black proportion: ", df_leaflet@data$black_prop,"<br/>",
                "Asian proportion: ", df_leaflet@data$asian_prop,"<br/>",
                "Hispanic proportion: ", df_leaflet@data$hispanic_prop,"<br/>",
                "White proportion: ", df_leaflet@data$white_prop,"<br/>",
                "Hawaiian/Pasifika proportion: ", df_leaflet@data$hawaiian_pi_prop,"<br/>",
                sep="") %>%
                lapply(htmltools::HTML)
            
            leafletProxy("leaflet_chloropleth") %>%
                clearShapes() %>%
                setView(-73.935242, 40.710310, zoom = 9.5) %>%
                #addProviderTiles(providers$CartoDB.Positron) %>%  #add simpler basemap
                #  clearShapes() %>%
                addPolygons(data =df_leaflet, 
                            fillColor = ~mypalette(target_date_pos$total),
                            fillOpacity = 0.5,
                            stroke=TRUE,
                            weight=0.8,
                            color="#000000",
                            label = mytag,
                            labelOptions = labelOptions(
                                style = list("font-weight" = "normal", padding = "3px 8px"),
                                textsize = "10px",
                                direction = "auto"),
                            highlight = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) %>%
                clearControls() %>%
                addLegend( pal=mypalette, values=target_date_pos$total, opacity=0.5, title = "Cases", position = "bottomleft" )
            
            
        }    
    } )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
