#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(sf)
load("climr_bris_data_84.RDA")

vars <- colnames(bris_geo_84)[c(7,8,10, 12)]

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("CLIMR example"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("variable",
                        "Select a variable:",
                        vars,
                        selected = vars[1])),

        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput("climrPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$climrPlot <- renderLeaflet({
        
        mapselect <- input$variable
        pal <- colorBin("YlOrRd", domain = as.numeric(bris_geo_84[[mapselect]]))
        
        if(input$variable == c("mean_age")){
            labels <- sprintf(
                "<strong>%s</strong><br/>%g years",
                bris_geo_84$msoa11nm, bris_geo_84$mean_age
            ) %>% lapply(htmltools::HTML)
        }
        
        if(input$variable == c("mean_wage")){
            labels <- sprintf(
                "<strong>%s</strong><br/>£%g  per hour",
                bris_geo_84$msoa11nm, bris_geo_84$mean_wage
            ) %>% lapply(htmltools::HTML)
        }
        
        if(input$variable == c("green_count")){
            labels <- sprintf(
                "<strong>%s</strong><br/>%g Green-ness",
                bris_geo_84$msoa11nm, bris_geo_84$green_count
            ) %>% lapply(htmltools::HTML)
            pal <- colorBin("Greens", domain = as.numeric(bris_geo_84[[mapselect]]))
        }
        
        if(input$variable == c("tas")){
            labels <- sprintf(
                "<strong>%s</strong><br/>%g Max temp",
                bris_geo_84$msoa11nm, bris_geo_84$tas
            ) %>% lapply(htmltools::HTML)
            pal <- colorBin("YlOrRd", domain = as.numeric(bris_geo_84[[mapselect]]))
        }
        
       leaflet(bris_geo_84) %>%
            setView(-2.6, 51.47, 11) %>%
            addTiles() %>%
            addPolygons(
                fillColor = ~pal(as.numeric(bris_geo_84[[mapselect]])),
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
           addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
                     position = "bottomright")
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
