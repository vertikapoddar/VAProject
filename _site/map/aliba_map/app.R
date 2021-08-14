library(shiny)


#The code chunk below is used to load and install all the necessary packages

packages = c('readr','ggiraph','igraph', 'tidyverse','plotly','lubridate','raster', 'sf', 'tmap','DT', 'greekLetters', 'clock', 'crosstalk', 'scales')
for (p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

#The code chunk below imports the data

gps_data <- read_csv("C:/vertikapoddar/Assignment/data/gps.csv")

#The code chunk below will perform the required changes

gps_data$Timestamp = mdy_hms(gps_data$Timestamp)
#gps_data$id = as_factor(gps_data$id)
#emp_data$CarID = as_factor(emp_data$CarID)
gps_data$id = as.character(gps_data$id)

bgmap <- raster("C:/vertikapoddar/data/MC2.tif")
bgmap

tm_shape(bgmap) + 
  tm_rgb(bgmap, r = 1, g = 2, b = 3,
         alpha = NA,
         saturation = 1,
         interpolate = TRUE,
         max.value = 255)

Abila_st <- st_read(dsn = "C:/vertikapoddar/Assignment/data/Geospatial",
                    layer = "Abila")

gps_sf <- st_as_sf(gps_data, 
                   coords = c("long", "lat"),
                   crs = 4326)


gps_path <- gps_sf %>%
  group_by(id) %>%
  summarize(m = mean(Timestamp),
            do_union = FALSE) %>%
  st_cast("LINESTRING")


gps_path_selected <- gps_path %>%
  filter(id == 1)
tmap_mode("view")
tm_shape(bgmap) +
  tm_rgb(bgmap, r = 1, g = 2, b = 3,
         alpha = NA,
         saturation = 1,
         interpolate = TRUE,
         max.value = 255) +
  tm_shape(gps_path_selected) +
  tm_lines()

# making a list of locations from cc_data
list_id <- as.list(gps_data$id)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Map path"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "carID",
                  label = "Select Car ID",
                  choices = list_id)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("location_day"),
      plotlyOutput("location_hour")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #the main graph  
  output$location_day <- renderPlotly({
    
    p <- cc_data %>% 
      filter(location %in% input$select_location) %>% 
      plot_ly(x= ~day)
    
  }) 
  
  # sub-graph   
  output$location_hour <- renderPlotly({
    d <- event_data("plotly_click")
    if (is.null(d)) return(NULL)
    
    p <- cc_data %>%
      filter(location %in% input$select_location) %>%
      filter(day %in% d$x) %>% 
      ggplot(aes(x=hour)) + 
      geom_bar() +
      scale_x_continuous(breaks= as.numeric(cc_data$hour))
    scale_y_continuous(breaks = as.numeric())
    ggplotly(p)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
