# GDP per Capita Dashboard

## Overview
This is an interactive Shiny dashboard that visualizes GDP per capita data across different continents and countries. The application allows users to explore statistics, compare continents, and analyze GDP rankings by country using various charts and tables.

## Features
- **Statistics Tab**: Displays summary statistics (mean, median, min, max, standard deviation) for GDP per capita by continent.
- **Continents Tab**: Shows a bar chart of total GDP per capita by continent.
- **Countries Tab**: Displays an ordered bar chart of GDP per capita for selected countries.
- **User Options**:
  - Year selection (1961-2021)
  - Continent selection
  - Number of top-ranked countries to display

## Technologies Used
- **R** (Programming Language)
- **Shiny** (Web Application Framework)
- **Shiny Dashboard** (Dashboard Layout)
- **Dplyr** (Data Manipulation)
- **Gapminder Dataset** (Source of GDP data)
- **GGPlot2** (Data Visualization)

## Installation
1. Ensure you have R and RStudio installed.
2. Install the required R packages:
    ```r
    install.packages(c("shiny", "shinydashboard", "dplyr", "gapminder", "tidyr", "stringr", "ECharts2Shiny", "ggplot2", "scales"))
    ```
3. Run the Shiny application:
    ```r
    library(shiny)
    runApp("app.R")
    ```

## Usage
1. Open the dashboard in your web browser.
2. Select a year, continent(s), and number of countries.
3. Explore interactive tables and charts.
4. Analyze GDP trends across different regions.

## Data Source
- The dataset used in this project is sourced from [Gapminder](https://www.gapminder.org/data/).


