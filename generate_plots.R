#!/usr/bin/env Rscript

# Function to check and install missing packages
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) {
    install.packages(new_packages, repos = "https://cran.rstudio.com/")
  }
}

# List of required packages
required_packages <- c("ggplot2", "dplyr", "lubridate", "zoo", "plotly", "ggdark", "htmlwidgets", "gridExtra", "readr")

# Install missing packages
install_if_missing(required_packages)

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(zoo)
library(plotly)
library(ggdark)
library(htmlwidgets)
library(gridExtra)
library(readr)

# Set working directory
set_working_directory <- function() {
  script_path <- commandArgs(trailingOnly = FALSE)
  script_path <- script_path[grep("--file=", script_path)]
  if (length(script_path) > 0) {
    script_path <- substring(script_path, 8)
    setwd(dirname(script_path))
  } else {
    setwd(getwd())
  }
  print(paste("Current working directory:", getwd()))
}

# Preprocess data
preprocess_data <- function(file_path) {
  data <- read_csv(file_path, col_types = cols(
    SampleNoText = col_character(),
    Arrived = col_character(),
    TestDate = col_character(),
    CustomerName = col_character(),
    SampleName = col_character(),
    MatrixCategory = col_character(),
    MatrixType = col_character(),
    SampleNo = col_double()
  ))
  data$Arrived <- as.POSIXct(data$Arrived, format = "%m/%d/%Y")
  data$Arrived <- as.Date(data$Arrived)
  data <- data %>% filter(!is.na(Arrived))
  return(data)
}


# Create total data plot
create_total_data_plot <- function(data) {
  data <- data %>%
    mutate(Arrived = as.Date(Arrived), # Ensure Arrived is in Date format
           Year = lubridate::year(Arrived)) # Extract Year from Arrived

  # Extend data to current date
  last_date <- max(data$Arrived)
  current_date <- Sys.Date()

  if (last_date < current_date) {
    date_sequence <- seq(last_date + 1, current_date, by = "day")
    extended_data <- data.frame(
      Arrived = date_sequence,
      NumSamples = 0
    )
    total_data_agg <- bind_rows(
      data %>% count(Arrived, name = "NumSamples"),
      extended_data
    ) %>% arrange(Arrived)
  } else {
    total_data_agg <- data %>% count(Arrived, name = "NumSamples") %>% arrange(Arrived)
  }

  total_data_agg <- data %>%
    count(Arrived, name = "NumSamples") %>%
    arrange(Arrived) %>%
    mutate(MovingAverage = rollmean(NumSamples, k = 30, fill = NA, align = "right"),
           StdDev = rollapply(NumSamples, width = 30, FUN = sd, fill = NA, align = "right"))

  lowest_per_year <- total_data_agg %>%
    mutate(Year = year(Arrived)) %>%
    group_by(Year) %>%
    slice(which.min(MovingAverage)) %>%
    ungroup()

  p <- plot_ly() %>%
  layout(plot_bgcolor = '#282a36', paper_bgcolor = '#282a36') %>%
  add_trace(data = total_data_agg, x = ~Arrived, y = ~NumSamples, type = 'scatter', mode = 'markers',
            marker = list(color = '#6272a4', opacity = 0.3), name = 'Daily Sample Arrivals') %>%
  add_trace(data = total_data_agg, x = ~Arrived, y = ~MovingAverage, type = 'scatter', mode = 'lines',
            line = list(color = '#50fa7b'), name = '30-Day Moving Average') %>%
  add_trace(data = lowest_per_year, x = ~Arrived, y = ~MovingAverage, type = 'scatter', mode = 'markers',
              marker = list(color = 'red', size = 10), name = 'Lowest Moving Average per Year') %>%
  add_ribbons(data = total_data_agg, x = ~Arrived, ymin = ~MovingAverage - StdDev, ymax = ~MovingAverage + StdDev, 
              line = list(color = 'rgba(139, 233, 253, 0.2)'), fillcolor = 'rgba(139, 233, 253, 0.3)', 
              name = '1 Std Dev') %>%
  add_ribbons(data = total_data_agg, x = ~Arrived, ymin = ~MovingAverage - 2 * StdDev, ymax = ~MovingAverage + 2 * StdDev, 
              line = list(color = 'rgba(255, 184, 108, 0.2)'), fillcolor = 'rgba(255, 184, 108, 0.1)', 
              name = '2 Std Dev') %>%
  add_ribbons(data = total_data_agg, x = ~Arrived, ymin = ~MovingAverage - 3 * StdDev, ymax = ~MovingAverage + 3 * StdDev, 
              line = list(color = 'rgba(255, 85, 85, 0.2)'), fillcolor = 'rgba(255, 85, 85, 0.1)', 
              name = '3 Std Dev') %>%
  layout(title = list(text = '30-Day Moving Average with Standard Deviations and Daily Sample Arrivals', font = list(color = '#f8f8f2')),
         xaxis = list(title = list(text = 'Arrival Date', font = list(color = '#f8f8f2')), tickfont = list(color = '#f8f8f2')),
         yaxis = list(title = list(text = 'Sample Count', font = list(color = '#f8f8f2')), tickfont = list(color = '#f8f8f2')),
         legend = list(font = list(color = '#f8f8f2')))
    
  return(p)
}

# Main script
main <- function() {
  set_working_directory()
  
  tryCatch({
    data <- preprocess_data("DataQueryExport_CN_240701.csv")
  
    p <- create_total_data_plot(data)
    saveWidget(p, "total_data_plot.html", selfcontained = FALSE)
  
    print("Plot and data have been exported successfully!")
  }, error = function(e) {
    print(paste("An error occurred:", e$message))
  }, warning = function(w) {
    print(paste("A warning occurred:", w$message))
  })
}

# Run main function
main()
