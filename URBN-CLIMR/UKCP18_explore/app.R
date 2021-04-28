library(shiny)
library(bslib)
library(fst)
library(googledrive)
library(dplyr)
library(ggplot2)
library(ggridges)
library(forcats)
#library(qs)

# drive_auth()
# files <- drive_find(q = c("starred = true"))
# files$id[2]
# 
# saveRDS(files$id[2], "qs_drive_id_filt.RDS")

#token <- readRDS("token.RDS")


# #read_fst(unzip(drop_download("01_df_shiny.fst.zip")))
drive_deauth()
x <- readRDS("qs_drive_id.RDS")
if(!file.exists("01_df_shiny.fst.zip")){
    drive_download(as_id(x), overwrite = T)
}

quantiles <- c("0","0.025","0.05","0.075","0.1","0.2","0.3",
               "0.4","0.5","0.6","0.7","0.8","0.9","0.925",
               "0.95","0.975","1","mean","sd") 
# quantiles <- c("0","0.025","0.05","0.5",
#               "0.95","0.975","1")

available_year <- c(1981:2000, 2021:2040, 2061:2080)

#df <- qread("01_df_shiny.qs")


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    theme = bslib::bs_theme(version = 4, bootswatch = "minty"),
    
    # Application title
    titlePanel("UKCP18 Data"),

    sidebarLayout(
        sidebarPanel(
            selectInput("quantile", "Quantile:",
                        quantiles, 
                        selected = 0.5),
            selectInput("first_year", "First year:",
                           available_year,
                        selected = 2030),
            selectInput("second_year", "Second year:",
                        available_year,
                        selected = 2070),
            position = "right", width = 3),

        mainPanel(
           plotOutput("ukcpPlot", height = 700, width = 800)
        )
    )
)
# Define server logic required to draw a histogram
server <- function(input, output) {

    output$ukcpPlot <- renderPlot({

        df_fil <- read_fst(unzip("01_df_shiny.fst.zip")) %>%  
            dplyr::select(-msoa) %>% 
            dplyr::filter(year %in% c(input$first_year, input$second_year) & stat == input$quantile)

        ggplot(data = df_fil, aes(y = fct_rev(month)))+
            geom_density_ridges(aes(x = value, fill = year), alpha = 0.6, colour = "transparent") +
            scale_fill_manual(values = c("#DAA03DFF", "#616247FF"), labels = c(input$first_year, input$second_year)) +
            theme_ridges(grid = T, center_axis_labels = TRUE)+
            labs(y = "Month", x = expression(Temperature^{o}*C), 
                 fill = "Year", title = "Maximum Temperatures across UK MSOAs", 
                 subtitle = ifelse(input$quantile %in% c("mean", "sd"),
                                   paste0("Data showing the ", input$quantile),
                                   paste0("Data showing the ", input$quantile, " quantile")))+
            theme(text = element_text(size=18))
    }) %>% 
        bindCache(input$first_year, input$second_year, input$quantile)
}

# Run the application 
shinyApp(ui = ui, server = server)
