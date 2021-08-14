#The code chunk below is used to load and install all the necessary packages
library(shiny)
library(readr)
library(ggiraph)
library(igraph)
library(tidyverse)
library(plotly)
library(lubridate)
library(raster)
library(sf)
library(tmap)
library(DT)
library(clock)
library(crosstalk)
library(scales)
library(tm)
library(wordcloud)
library(memoise)

#The code chunk below imports the data

cc_data <- read_csv("data/cc_data.csv")


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

### 2.2 Setting up Code

# 2.2.1 Set seed for reproducibility 
set.seed(1234) 

# 2.2.2 Import csv files
df1 <- read_csv("data/csv-1700-1830.csv")
df2 <- read_csv("data/csv-1831-2000.csv")
df3 <- read_csv("data/csv-2001-2131.csv")

colnames(df1)[2] <- c("dt")
colnames(df2)[2] <- c("dt")
colnames(df3)[2] <- c("dt")

# 2.2.3 Change message to lowercase
df1$message <- tolower(df1$message)
df2$message <- tolower(df2$message)
df3$message <- tolower(df3$message)

# 2.2.4 Change datetime
df1$dt = df1$dt - 20140123000000
df2$dt = df2$dt - 20140123000000
df3$dt = df3$dt - 20140123000000

df1$dt <- format(strptime(df1$dt, format="%H%M%S"), format = "%H:%M:%S")
df2$dt <- format(strptime(df2$dt, format="%H%M%S"), format = "%H:%M:%S")
df3$dt <- format(strptime(df3$dt, format="%H%M%S"), format = "%H:%M:%S")

# 2.2.5 Create new dataframes
# df1 17:00 to <18:30
# df2 18:30 to <20:00
# df3 >=20:00
# df4 17:00:00 to 21:34:45

# Bind df1, df2 and df3 together to create df4
df4 <- rbind(df1, df2, df3)

# df1: Extract 17:00 to <18:30
df1 <- df4[df4$dt < "18:30:00",]
df1$period <- "17:00 to <18:30"

# df2: Extract 18:30 to <20:00
df2 <- df4[df4$dt >= "18:30:00" & df4$dt < "20:00:00",]
df2$period <- "18:30 to <20:00"

# df3: Extract >=20:00
df3 <- df4[df4$dt >= "20:00:00",]
df3$period <- ">=20:00"

# Remove Spam
# Remove KronosQuoth
df1 <- df1 %>%
  filter(!str_detect(author, 'KronosQuoth'))
df2 <- df2 %>%
  filter(!str_detect(author, 'KronosQuoth'))
df3 <- df3%>%
  filter(!str_detect(author, 'KronosQuoth'))
df4 <- df4%>%
  filter(!str_detect(author, 'KronosQuoth'))

# Remove RTs
df1 <- df1 %>%
  filter(!str_detect(message, 'rt @'))
df2 <- df2 %>%
  filter(!str_detect(message, 'rt @'))
df3 <- df3%>%
  filter(!str_detect(message, 'rt @'))
df4 <- df4%>%
  filter(!str_detect(message, 'rt @'))

# List of DF
# df1 17:00 to <18:30
# df2 18:30 to <20:00
# df3 >=20:00
# df4 17:00:00 to 21:34:45

df1$period <- "17:00 to <18:30"
df2$period <- "18:30 to <20:00"
df3$period <- "20:00 to 21:34"
df4$period <- "Whole Period"
dfall <- rbind(df1, df2, df3, df4)
keep <- c("message","period")
dfall <- dfall[keep]

# DF1
text <- df1$message
text <- str_replace_all(text,"@[a-z,A-Z]*","") # Remove @
text <- str_replace_all(text,"abila","") # Remove abila
text <- str_replace_all(text,"pok","") # Remove pok
text <- str_replace_all(text,"abilapost","") # Remove abilapost
text <- str_replace_all(text,"kronosstar","") # Remove kronosstar

docs <- Corpus(VectorSource(text))
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
wc1 <- data.frame(word = names(words),freq=words)

# DF2
text <- df2$message
text <- str_replace_all(text,"@[a-z,A-Z]*","") # Remove @
text <- str_replace_all(text,"abila","") # Remove abila
text <- str_replace_all(text,"pok","") # Remove pok
text <- str_replace_all(text,"abilapost","") # Remove abilapost
text <- str_replace_all(text,"kronosstar","") # Remove kronosstar

docs <- Corpus(VectorSource(text))
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
wc2 <- data.frame(word = names(words),freq=words)

# DF3
text <- df3$message
text <- str_replace_all(text,"@[a-z,A-Z]*","") # Remove @
text <- str_replace_all(text,"abila","") # Remove abila
text <- str_replace_all(text,"pok","") # Remove pok
text <- str_replace_all(text,"abilapost","") # Remove abilapost
text <- str_replace_all(text,"kronosstar","") # Remove kronosstar

docs <- Corpus(VectorSource(text))
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
wc3 <- data.frame(word = names(words),freq=words)

# DF4
text <- df4$message
text <- str_replace_all(text,"@[a-z,A-Z]*","") # Remove @
text <- str_replace_all(text,"abila","") # Remove abila
text <- str_replace_all(text,"pok","") # Remove pok
text <- str_replace_all(text,"abilapost","") # Remove abilapost
text <- str_replace_all(text,"kronosstar","") # Remove kronosstar

docs <- Corpus(VectorSource(text))
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))
docs

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
words
wc4 <- data.frame(word = names(words),freq=words)

wc1$period <- "17:00 to <18:30"
wc2$period <- "18:30 to <20:00"
wc3$period <- "20:00 to 21:34"
wc4$period <- "Whole Period"
wcall <- rbind(wc1, wc2, wc3, wc4)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Creating tabs
      tabsetPanel(
        
        tabPanel("Word Cloud",
                 
                 titlePanel("Word Cloud"),
                 
                 sidebarLayout(
                   # Sidebar with a slider and selection inputs
                   sidebarPanel(
                     selectInput("selection", "Choose a Period:",
                                 choices = c("17:00 to <18:30",
                                             "18:30 to <20:00",
                                             "20:00 to 21:34",
                                             "Whole Period")),
                     actionButton("update", "Update Period"),
                     p(),
                     em("Note: Wordcloud Period will not update unless the Update button is pressed", align = "center"),
                     hr(),
                     sliderInput("freq",
                                 "Minimum Frequency:",
                                 min = 1,  max = 50, value = 15),
                     sliderInput("max",
                                 "Maximum Number of Words:",
                                 min = 1,  max = 300,  value = 100)
                   ),
                   
                   # Show Word Cloud
                   mainPanel(
                     plotOutput("plot"),
                     tableOutput("plot2")
                   )
                 )      
                 
        ),
        tabPanel("Location Popularity",

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
    )),
      
      tabPanel("Network Graph")
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
   df_wc <- dfall
   df_ct <- wcall
   filtered <- reactive(filter(df_wc, period == input$selection))
   
   wc_data <- reactive({
     input$update
     isolate({
       wc_file <- filter(df_wc, period == input$selection)
       wc_text <- wc_file$message
       wc_text <- str_replace_all(wc_text,"@[a-z,A-Z]*","") # Remove @
       wc_text <- str_replace_all(wc_text,"abila","") # Remove abila
       wc_text <- str_replace_all(wc_text,"pok","") # Remove pok
       wc_text <- str_replace_all(wc_text,"abilapost","") # Remove abilapost
       wc_text <- str_replace_all(wc_text,"kronosstar","") # Remove kronosstar
       wc_corpus <- Corpus(VectorSource(wc_text))
       wc_corpus_clean <- wc_corpus %>%
         tm_map(removeNumbers) %>%
         tm_map(removePunctuation) %>%
         tm_map(stripWhitespace)
       wc_corpus_clean <- tm_map(wc_corpus_clean, content_transformer(tolower))
       wc_corpus_clean <- tm_map(wc_corpus_clean, removeWords, stopwords("english"))
     })
   })
   
   wordcloud_rep <- repeatable(wordcloud)
   output$plot <- renderPlot({
     wc_corpus <- wc_data()
     wordcloud(wc_corpus, 
               min.freq = input$freq,
               max.words=input$max, random.order=FALSE, rot.per=0.35,
               colors=brewer.pal(8, "Dark2"))
   })
   
   output$plot2 <- renderTable(filtered2 <- filter(df_ct, period == input$selection & freq >= input$freq))

}

# Run the application 
shinyApp(ui = ui, server = server)
