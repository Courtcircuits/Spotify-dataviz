library(shiny)
library(here)
library(bslib)
library(readr)
library(plotly)

top50 <- read_csv(here("data", "playlist_2010to2023.csv"))
spec(top50)


genres <- top50$artist_genres

# Remove the square brackets and split the strings into individual genres
genres <- gsub("\\[|\\]", "", genres) # Remove square brackets
genres <- unlist(strsplit(genres, ", ")) # Split the strings and unlist
# only get unique genres
genres <- unique(genres)

years <- unique(top50$year)


# Define UI for app that draws a histogram ----
ui <- page_sidebar(
    # App title ----
    title = "What makes a song popular ?",
    # Sidebar panel for inputs ----
    sidebar = sidebar(
        # Input: Slider for the number of bins ----
        sliderInput(
            inputId = "sampleSize",
            label = "Sample Size:",
            min = 10, max = top50 %>% nrow(),
            value = 10
        ),
        checkboxGroupInput(
            inputId = "years",
            label = "Années sélectionnées:",
            choices = years,
            selected = years
        )
    ),
    # Output: Histogram ----
    tabsetPanel(
        tabPanel(
            "TSNE",
            plotlyOutput("tsnePlot")
        ),
        tabPanel("Modalities against Popularity", fluidRow(
            column(6, plotOutput("dotPlot")),
            column(6, plotOutput("dotPlot2"))
        )),
        tabPanel(
            "Trends over the years",
            column(12, plotOutput("timeSeries")),
            column(12, plotOutput("timeSeriesDuration")),
            column(12, plotOutput("timeSeriesTempo")),
            column(12, plotOutput("timeSeriesMultiModals")),
        )
    )
)
