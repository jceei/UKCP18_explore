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
library(dplyr)
#load("climr_bris_data_84.RDA")
bristol_data<- readRDS("dummy_data_bristol.RDS")

exposure <- bristol_data %>% 
    select(msoa11cd, exposure) 

vulnerability <- bristol_data %>% 
    select(msoa11cd, vulnerability) 

hazard <- bristol_data %>% 
    select(msoa11cd, hazard) 

risk <- bristol_data %>% 
        select(msoa11cd, risk) 


pal_exposure <- colorBin("RdPu", domain = as.numeric(exposure$exposure), na.color = "transparent")
pal_vulnerability <- colorBin("Purples", domain = as.numeric(vulnerability$vulnerability), na.color = "transparent")
pal_hazard <- colorBin("Reds", domain = as.numeric(hazard$hazard), na.color = "transparent")
pal_risk <- colorBin("YlOrRd", domain = as.numeric(risk$risk), na.color = "transparent")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    theme = bslib::bs_theme(version = 4, bootswatch = "minty"),

    # Application title
    titlePanel("CLIMR example"),

        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput("climrPlot", height=600, width = 900)
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$climrPlot <- renderLeaflet({
        
    labels <- sprintf(
        "<strong>%s</strong><br/>Exposure: %g <br/>Vulnerability: %g <br/>Hazard: %g <br/>Risk: %g" ,
        bristol_data$msoa11nm, bristol_data$exposure,bristol_data$vulnerability,
        bristol_data$hazard, bristol_data$risk
    ) %>% lapply(htmltools::HTML)
        
       leaflet(bristol_data) %>%
            setView(-2.6, 51.47, 11) %>%
            addTiles() %>%
                      addPolygons(data = exposure,
                        fillColor = pal_exposure(exposure$exposure),
                group = "Exposure",
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.8,
                highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.5,
                    bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>% 
            addPolygons(data = vulnerability,
                        fillColor = pal_vulnerability(vulnerability$vulnerability),
                        group = "Vulnerability",
                        weight = 2,
                        opacity = 1,
                        color = "white",
                        dashArray = "3",
                        fillOpacity = 0.8,
                        highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.5,
                            bringToFront = TRUE),
                        label = labels,
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "15px",
                            direction = "auto")) %>% 
            addPolygons(data = hazard,
                        fillColor = pal_hazard(hazard$hazard),
                        group = "Hazard",
                        weight = 2,
                        opacity = 1,
                        color = "white",
                        dashArray = "3",
                        fillOpacity = 0.8,
                        highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.5,
                            bringToFront = TRUE),
                        label = labels,
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "15px",
                            direction = "auto")) %>% 
            addPolygons(data = risk,
                        fillColor = pal_risk(risk$risk),
                        group = "Risk",
                        weight = 2,
                        opacity = 1,
                        color = "white",
                        dashArray = "3",
                        fillOpacity = 0.8,
                        highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.5,
                            bringToFront = TRUE),
                        label = labels,
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "15px",
                            direction = "auto")) %>% 
       addLayersControl(
           overlayGroups = c("Exposure", "Vulnerability", "Hazard", "Risk"),
           options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)
       )
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
