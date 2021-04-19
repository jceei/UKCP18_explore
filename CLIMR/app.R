library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(thematic)
library(bslib)
#load("climr_bris_data_84.RDA")

bristol_data <- readRDS("dummy_data_bristol.RDS") %>%
    mutate(hazard = hazard_sc)

years <- unique(bristol_data$year)
risk_empty <- unique(bristol_data %>% select(msoa11cd))

vulnerability <- unique(bristol_data %>%
                            dplyr::select(msoa11cd, vulnerability))

hazard <- bristol_data %>%
    dplyr::select(msoa11cd, hazard = hazard_sc, year)

# risk <- bristol_data %>%
#     dplyr::select(msoa11cd, risk, year)


thematic::thematic_shiny()
ui <- fluidPage(
    theme = bslib::bs_theme(version = 4, bootswatch = "minty"),
    
    tags$head(tags$link(rel = "shortcut icon", href = "JCEEI_logo.png")),
    
    navbarPage(
        "Bristol Climate Heat Risk",
        tabPanel(
            "Map",
            titlePanel(title = "", windowTitle = "Urban Heat"),
            # Show a plot of the generated distribution,
            sidebarLayout(
                position = "right",
                sidebarPanel(
                    selectInput("year",
                                "Hazard - select year:",
                                years,
                                selected = "2076"),
                    sliderInput(
                        "exposure",
                        "Exposure - select % population exposed:",
                        min = 0,
                        max = 100,
                        value = 100
                    ),
                    downloadButton("downloadData", "Download .csv"),
                    width = 3
                ),
                # Application title
                mainPanel(
                    leafletOutput("climrPlot", height = 600, width = 900),
                    img(
                        src = "Final_logo_with_space.png",
                        width = "174.4",
                        height = "65.2",
                        align = "right"
                    ),
                    
                )
            )
        ),
        tabPanel("Data",
                 # sidebarLayout(
                 #     # Sidebar panel for inputs ----
                 #     sidebarPanel(# Button
                 #         downloadButton("downloadData", "Download")),  # doesn't seem to like multiple download buttons
                 #        
                     # Main panel for displaying outputs ----
                     mainPanel(tableOutput("outputTable"))
                 )
    )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
    output$climrPlot <- renderLeaflet({
        dataInput <- bristol_data[bristol_data$year == input$year, ]
        dataInput$exposure <- input$exposure / 100
        dataInput$risk <-
            dataInput$hazard * dataInput$vulnerability * dataInput$exposure
        
        pal_exposure <-
            colorBin("RdPu", domain = c(0, 1), na.color = "transparent")
        pal_vulnerability <-
            colorBin("Purples",
                     domain = c(
                         min(bristol_data$vulnerability),
                         max(bristol_data$vulnerability)
                     ),
                     na.color = "transparent")
        pal_hazard <-
            colorBin("Reds",
                     domain = c(min(bristol_data$hazard), max(bristol_data$hazard)),
                     na.color = "transparent")
        pal_risk <-
            colorBin("YlOrRd",
                     domain = c(0, 1),
                     na.color = "transparent")
        
        labels <- sprintf(
            "<strong>%s</strong><br/>Exposure: %g <br/>Vulnerability: %g <br/>Hazard: %g <br/>Risk: %g" ,
            dataInput$msoa11nm,
            dataInput$exposure,
            dataInput$vulnerability,
            dataInput$hazard,
            dataInput$risk
        ) %>% lapply(htmltools::HTML)
        
        leaflet(risk_empty) %>%
            setView(-2.6, 51.47, 11) %>%
            addTiles() %>%
            addPolygons(
                data = risk_empty,
                fillColor = pal_exposure(dataInput$exposure),
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
                    bringToFront = TRUE
                ),
                label = labels,
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"
                )
            ) %>%
            addPolygons(
                data = risk_empty,
                fillColor = pal_vulnerability(dataInput$vulnerability),
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
                    bringToFront = TRUE
                ),
                label = labels,
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"
                )
            ) %>%
            addPolygons(
                data = risk_empty,
                fillColor = pal_hazard(dataInput$hazard),
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
                    bringToFront = TRUE
                ),
                label = labels,
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"
                )
            ) %>%
            addPolygons(
                data = risk_empty,
                fillColor = pal_risk(dataInput$risk),
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
                    bringToFront = TRUE
                ),
                label = labels,
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"
                )
            ) %>%
            addLayersControl(
                overlayGroups = c("Exposure", "Vulnerability", "Hazard", "Risk"),
                options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)
            )
        
    })
    
    
    datasetInput <-  reactive({
        dataInput <- bristol_data[bristol_data$year == input$year, ]
        dataInput$exposure <- input$exposure / 100
        dataInput$risk <-
            dataInput$hazard * dataInput$vulnerability * dataInput$exposure
        
        as_tibble(dataInput) %>%
            select(year,
                   msoa11cd,
                   msoa11nm,
                   exposure,
                   vulnerability,
                   hazard,
                   risk) %>%
            arrange(msoa11nm)
    })
    
    output$outputTable <- renderTable({
        datasetInput()
    })
    
    output$downloadData <- downloadHandler(
        filename = paste(
            "bristol_",
            input$year,
            "_exposure_",
            input$exposure ,
            ".csv",
            sep = ""
        ),
        content = function(file) {
            write.csv(datasetInput(), file, row.names = FALSE)
        }
    )
}

# Run the application
shinyApp(ui = ui, server = server)
