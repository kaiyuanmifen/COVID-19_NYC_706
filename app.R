library(shiny)
library(leaflet)


#Load data 

#County data
nycounties <- rgdal::readOGR("virus_data/gz_2010_us_050_00_20m.geojson")
head(nycounties)
nycounties

class(nycounties)
nycounties$GEO_ID=as.character(nycounties$GEO_ID[nycounties$STATE=="36"])


length(nycounties$COUNTY)
nycounties$COVID_Case=0
nycounties$COVID_Rate=0
head(nycounties)
pal <- colorNumeric("viridis", NULL)

#case data 
Cases=read.csv("virus_data/expanded_git_dir/1587580105-9147df0/boro.csv")
Cases=Cases[1:5,]
Cases$County=c("Bronx","Kings","New York","Queens","Richmond")##Borough to county

for (i in 1:length(Cases$County)){
    nycounties$COVID_Case[(nycounties$STATE=="36")&(nycounties$NAME==Cases$County[i])]=Cases$COVID_CASE_COUNT[i]
    nycounties$COVID_Rate[(nycounties$STATE=="36")&(nycounties$NAME==Cases$County[i])]=Cases$COVID_CASE_RATE[i]
}


r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

#UI-server 

ui <- fluidPage(
    leafletOutput("mymap"),
    p(),
    sliderInput(inputId = "Date_num", 
                label = "Choose a day", 
                value = 25, min = 1, max = 100),
    selectInput("DataType", "Data type",c("case counts","case rate")),
    plotOutput("hist")
    
)

server <- function(input, output, session) {
    
   
    
    observe({
    #nycounties$Target=nycounties$COVID_Case
    if(input$DataType=="case counts"){
        nycounties$Value=nycounties$COVID_Case
    }    
        
    if(input$DataType=="case rate"){
        nycounties$Value=nycounties$COVID_Rate
    }
    
        
    output$mymap <- renderLeaflet({
        m=leaflet(nycounties) %>%
            addTiles() %>%
            addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.5,
                        fillColor = ~pal(Value), opacity = 0.5,
                        label = ~paste0(NAME, ": ", formatC(Value, big.mark = ","))) %>%
            addLegend(pal = pal, values = ~Value, opacity = 1,
                      labFormat = labelFormat(transform = function(x) round(x)))
        
        setView(m,lng=-74.005974, lat=40.7128,zoom = 12)
    })
    
    output$hist <- renderPlot({
        barplot(nycounties$Value[(nycounties$STATE=="36")&(nycounties$NAME%in%Cases$County)],main="Values",
                col=1:sum((nycounties$STATE=="36")&(nycounties$NAME%in%Cases$County)))
    })
    
    })
    
    #observe click events 
    observeEvent(input$mymap_marker_click, { 
        p <- input$map_marker_click
        print(p)
    })
    
    
}

shinyApp(ui, server)
