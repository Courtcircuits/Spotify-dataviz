# Define server logic required to draw a histogram ----
library(Rtsne)
library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)
library(tsne)
library(tidyr)

dataset_raw <- read_csv(here("data", "playlist_2010to2023.csv"))
top50 <- unique(dataset_raw)


ms_to_min <- function(ms) {
    return(ms / 60000)
}

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

    output$timeSeriesDuration <- renderPlot({
        dataset <- data.frame(
            year = top50$year,
            duration = ms_to_min(top50$duration_ms)
        )

        dataset <- dataset %>% slice_sample(n = input$sampleSize)

        print(dataset)

        aggregated_data <- dataset %>%
            group_by(year) %>%
            summarise(avg_duration = mean(duration)) %>%
            ungroup()

        print(aggregated_data)

        ggplot(aggregated_data, aes(x = year, y = avg_duration)) +
            geom_line(color = "blue") +
            geom_point(color = "red") +
            labs(
                title = "Duration over the years",
                x = "Year",
                y = "Duration in minutes"
            ) +
            theme_minimal()
    })

    output$timeSeriesTempo <- renderPlot({
        dataset <- data.frame(
            year = top50$year,
            tempo = top50$tempo
        )

        dataset <- dataset %>% slice_sample(n = input$sampleSize)

        print(dataset)

        aggregated_data <- dataset %>%
            group_by(year) %>%
            summarise(avg_tempo = mean(tempo)) %>%
            ungroup()

        print(aggregated_data)

        ggplot(aggregated_data, aes(x = year, y = avg_tempo)) +
            geom_line(color = "blue") +
            geom_point(color = "red") +
            labs(
                title = "Tempo over the years",
                x = "Year",
                y = "Tempo"
            ) +
            theme_minimal()
    })


    output$timeSeriesMultiModals <- renderPlot({
        dataset <- data.frame(
            year = top50$year,
            danceability = top50$danceability,
            energy = top50$energy,
            speechiness = top50$speechiness,
            acousticness = top50$acousticness,
            instrumentalness = top50$instrumentalness,
            liveness = top50$liveness
        )

        dataset <- dataset %>% slice_sample(n = input$sampleSize)

        print(dataset)

        aggregated_data <- dataset %>%
            group_by(year) %>%
            summarise(
                avg_danceability = mean(danceability),
                avg_energy = mean(energy),
                avg_speechiness = mean(speechiness),
                avg_acousticness = mean(acousticness),
                avg_instrumentalness = mean(instrumentalness),
                avg_liveness = mean(liveness)
            ) %>%
            ungroup()

        aggregated_data <- pivot_longer(aggregated_data, cols = c("avg_danceability", "avg_energy", "avg_speechiness", "avg_acousticness", "avg_instrumentalness", "avg_liveness"), names_to = "feature", values_to = "value")

        print(aggregated_data)


        ggplot(aggregated_data, aes(x = year, y = value, color = feature)) +
            geom_line() +
            geom_point() +
            labs(
                title = "Features over the years",
                x = "Year",
                y = "Value",
                color = "Feature"
            ) +
            theme_minimal()
    })
}
