# Define server logic required to draw a histogram ----
library(Rtsne)
library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)
library(tsne)

dataset_raw <- read_csv(here("data", "playlist_2010to2023.csv"))
top50 <- unique(dataset_raw)



normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

server <- function(input, output) {
  output$tsnePlot <- renderPlotly({
    ########
    # INPUTS#
    ########
    sample_size <- input$sampleSize
    years <- input$years

    top_50_filtered <- top50 %>% filter(year %in% years)
    dataset_track_name <- data.frame(
      track_name = paste0(top_50_filtered$artist_name, " - ", top_50_filtered$track_name),
      key = normalize(top_50_filtered$key),
      loudness = normalize(top_50_filtered$loudness),
      tempo = normalize(top_50_filtered$tempo),
      duration_ms = normalize(top_50_filtered$duration_ms)
    )

    dataset_track_name <- dataset_track_name %>% slice_sample(n = sample_size)


    features <- subset(dataset_track_name, select = -c(track_name))
    set.seed(0)
    tsne <- tsne(features, initial_dims = 2)
    tsne <- data.frame(tsne)
    pdb <- cbind(tsne, dataset_track_name$track_name)

    fig <- plot_ly(data = pdb, x = ~X1, y = ~X2, mode = "markers", split = ~ dataset_track_name$track_name)
    fig %>%
      layout(
        plot_bgcolor = "#e5ecf6",
        height = 800
      )
  })

  output$dotPlot <- renderPlot({
    dataset <- data.frame(
      title = top50$track_name,
      duration = top50$duration_ms,
      popularity = top50$track_popularity,
      modes = top50$mode
    )

    dataset <- dataset %>% slice_sample(n = input$sampleSize)
    dataset$color <- ifelse(dataset$mode == 1, "red", "blue")

    ggplot(dataset, aes(x = duration, y = popularity, color = color)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      scale_color_identity() +
      labs(
        title = "Duration vs Popularity",
        x = "Duration (ms)",
        y = "Popularity",
        color = "Mode"
      ) +
      theme_minimal()
  })
  output$dotPlot2 <- renderPlot({
    dataset <- data.frame(
      title = top50$track_name,
      artist_popularity = top50$artist_popularity,
      popularity = top50$track_popularity,
      modes = top50$mode
    )

    dataset <- dataset %>% slice_sample(n = input$sampleSize)
    dataset$color <- ifelse(dataset$mode == 1, "red", "blue")

    ggplot(dataset, aes(x = artist_popularity, y = popularity, color = color)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      scale_color_identity() +
      labs(
        title = "Artist Popularity vs Popularity",
        x = "Artist Popularity",
        y = "Popularity",
        color = "Mode"
      ) +
      theme_minimal()
  })
  output$happyHistogram <- renderPlot({
    dataset <- data.frame(
      title = top50$track_name,
      happiness = top50$valence,
      mode = top50$mode
    )

    dataset <- dataset %>% slice_sample(n = input$sampleSize)
    # dataset$color <- ifelse(dataset$mode == 1, "red", "blue")

    aggregated_data <- dataset %>%
      group_by(happiness, mode) %>%
      summarise(count = n()) %>%
      ungroup()



    # Create a complete dataset for both modes

    ggplot(aggregated_data, aes(x = happiness, y = count, color = factor(mode))) +
      geom_line() +
      scale_color_manual(
        values = c("blue", "red"),
        labels = c("Minor", "Major"),
        name = "Mode"
      ) +
      labs(
        title = "Number of Musics by Valence",
        x = "Valence (Happiness)",
        y = "Number of Musics"
      ) +
      theme_minimal()
  })

  output$timeSeries <- renderPlot({
    dataset <- data.frame(
      year = top50$year,
      popularity = top50$track_popularity
    )

    dataset <- dataset %>% slice_sample(n = input$sampleSize)

    print(dataset)

    aggregated_data <- dataset %>%
      group_by(year) %>%
      summarise(avg_popularity = mean(popularity)) %>%
      ungroup()

    print(aggregated_data)

    ggplot(aggregated_data, aes(x = year, y = avg_popularity)) +
      geom_line(color = "blue") +
      geom_point(color = "red") +
      labs(
        title = "Popularity over the years",
        x = "Year",
        y = "Popularity"
      ) +
      theme_minimal()
  })

  output$timeSeriesGenres <- renderPlot({
    dataset <- data.frame()
  })
}
