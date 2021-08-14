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

cc_data <- read_csv("C:/vertikapoddar/Assignment/data/cc_data.csv")


#The code chunk below will perform the required changes

cc_data$timestamp = mdy_hm(cc_data$timestamp)
cc_data$date = get_day(cc_data$timestamp)
cc_data$hour = get_hour(cc_data$timestamp)


#sorting the cc_data by location
cc_data <- cc_data[order(cc_data$location),]

#getting the day of the week
cc_data$day <- wday(cc_data$timestamp, label = TRUE)

# making a list of locations from cc_data
list_location <- as.list(cc_data$location)



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Locations by frequency of vists"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "select_location",
                      label = "Select Location",
                      choices = list_location)
            
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
