library(shiny)
library(shinydashboard)
library(dplyr)
library(gapminder)
library(tidyr)
library(stringr)
library(ECharts2Shiny)
library(ggplot2)
library(scales)



ui <- dashboardPage(
  dashboardHeader(title = "GDP per capita"),
  dashboardSidebar(
    # dodanie menu bocznego
    sidebarMenu(
      menuItem(
        "Statistics",
        tabName = "Statistics",
        badgeLabel = "Statistics",
        badgeColor = "purple"
      ),
      menuItem(
        "Continents",
        tabName = "Continents",
        badgeLabel = "Continents",
        badgeColor = "olive"
      ),
      menuItem(
        "Countries",
        tabName = "Countries",
        badgeLabel = "Countries",
        badgeColor = "fuchsia"
      )
    ),
    # koniec menu
    # dodajemy wejście
    selectInput(
      'year',
      h3("Select year"),
      choices = c(1961:2021),
      multiple = FALSE,
      selected = 2022
    ),
    selectInput(
      'continent',
      h4("Select continent"),
      choices = c("Asia", "Africa", "Americas", "Europe", "Oceania"),
      multiple = TRUE,
      selected = c("Europe","Asia","Americas")
    ),
    sliderInput("slider", "Number of countries:", 1, 10, 5,ticks=FALSE)
    
  ),
  dashboardBody(# Boxes powinny być w wierszach lub kolumnach
    fluidRow(tabItems(
      tabItem(tabName = "Statistics",
              h2("Select year and continents",style="text-align:center;"),
              box(
                width =12,
                column(12, align="center", tableOutput('op'),
                       #tableOutput("op"),
                       plotOutput("plot1")
                       # wyświetlenie tekstu (kwantyli)
                ), column(1, align="left", uiOutput("tab")))),
      tabItem(
        tabName = "Continents",
        h2("Select year",style="text-align:center;"),
        box(
          width =12,
          column(12, align="center",
                 plotOutput("plot2", height = 600)
          ), column(1, align="left", uiOutput("tab2")))),
      tabItem(
        tabName = "Countries",
        h2("Select year, continent and number of countries", style="text-align:center;"),
        box(
          width =12,
          column(12, align="center",
                 plotOutput("plot3", height = 600)
          ), column(1, align="left", uiOutput("tab3"))))
    )))
)

server <- function(input, output) {
  
  # wczytanie danych
  g <- read.csv(file = "gdp.csv",
                head = TRUE)
  long_g <- gather(g, year, val, -country)
  
  long_g$year <-
    substr(long_g$year, 2, 5)
  
  long_g$year <-
    as.integer(long_g$year)
  
  total <- inner_join(long_g, gapminder, by = "country")
  
  colnames(total) <- c("country", "year", "gdp_pc", "continent")
  
  total <- total %>%
    select(c(1:4)) %>%
    distinct()
  
  
  kvar <- str_detect(total$gdp_pc, "k")
  
  total$gdp_pc <-
    ifelse(kvar,as.numeric(str_replace(total$gdp_pc, "k", "")) * 1000, as.numeric(total$gdp_pc))
  
  url <- a("gapminder", href="https://www.gapminder.org/data/")
  output$tab <- renderUI({
    tagList("source:", url)
  })
  output$tab2 <- renderUI({
    tagList("source:", url)
  })
  output$tab3 <- renderUI({
    tagList("source:", url)
  })
  
  # Select year
  output$op <- renderTable({
    stats <- total %>%
      filter(year == input$year & continent %in% input$continent) %>%
      group_by(continent, year) %>%
      arrange(-gdp_pc) %>%
      summarize(
        continent = continent,
        mean = round(mean(gdp_pc, na.rm = TRUE),digits=2),
        median = round(median(gdp_pc,na.rm = TRUE),digits=2),
        min = round(min(gdp_pc, na.rm = TRUE),digits=2),
        max = round(max(gdp_pc, na.rm = TRUE),digits=2),
        sd = round(sd(gdp_pc, na.rm = TRUE),digits=2)
      ) %>%
      arrange(-mean) %>%
      distinct()
  })
  
  #kwantyle
  output$plot1 <- renderPlot({
    
    stats <- total %>%
      filter(year == input$year & continent %in% input$continent & !(country %in% c("China","India"))) %>%
      group_by(continent, year) %>%
      distinct()
    
    ggplot(data = stats, aes(x = continent, y = gdp_pc)) +
      geom_boxplot(
        aes(fill=continent),
        alpha = 0.5,
        frame = FALSE,
        border = "black"
      ) +
      scale_y_continuous(labels = unit_format(unit = "k", scale = 1e-3))+
      scale_fill_manual(
        values = c(
          Asia = "Yellow",
          Africa = "Brown",
          Americas = "Red",
          Europe = "Orange",
          Oceania = "Blue"
        ))+
      labs(x = "Continent", y = "gdp_pc", title =
             "Boxplot by continent") +
      theme(plot.title = element_text(hjust = 0.5, size = 15, face = 'bold')) +  theme_bw() +
      theme(
        legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12,
                                  face = "bold"),
        plot.title = element_text(size = 18,
                                  face = "bold"),
        plot.subtitle = element_text(size = 12),
        legend.title = element_text(size = 14,
                                    face = "bold"),
        legend.text = element_text(size = 12)
      ) 
    
  })
  
  output$plot2 <- renderPlot({
    fin <- total %>%
      filter(!is.na(gdp_pc)) %>%  # Usuwanie wartości brakujących
      group_by(continent, year) %>%
      summarise(gdppc_total = sum(gdp_pc, na.rm = TRUE)) %>%
      filter(year == input$year)
    
    ggplot(fin, aes(
      x = gdppc_total,
      y = reorder(continent, gdppc_total),
      fill = continent
    )) +
      labs(
        x = "",
        y = "",
        title = "Total GDP per capita by continent",
        color = "Continents"
      ) +
      theme(plot.title = element_text(
        hjust = 0.5,
        size = 15,
        face = 'bold'
      )) +
      scale_x_continuous(labels = unit_format(unit = "m", scale = 1e-6)) +
      geom_bar(stat = "identity") +
      geom_text(
        aes(label = paste0(
          as.character(round(gdppc_total / 1000000,digits=2)), "m"
        )),
        vjust = 0.4,
        color = "black",
        size = 3.8,
        hjust = -0.05
      ) +
      scale_fill_manual(
        values = c(
          Asia = "Yellow",
          Africa = "Brown",
          Americas = "Red",
          Europe = "Orange",
          Oceania = "Blue"
        ))+
      theme_bw() +
      theme(
        legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12,
                                  face = "bold"),
        plot.title = element_text(size = 18,
                                  face = "bold"),
        plot.subtitle = element_text(size = 12),
      )
    
  })
  
  
  output$plot3 <- renderPlot({
    countries <- total %>%
      filter(continent %in% input$continent & year == input$year) %>%
      arrange(-gdp_pc) %>%
      head(input$slider) %>%
      arrange(-gdp_pc)
    
    ggplot(countries, aes(
      x = gdp_pc,
      y = reorder(country, gdp_pc),
      fill = continent
    )) +
      labs(
        x = "gdp_pc",
        y = "Country",
        title = "GDP per capita by country",
        color = "Continents"
      ) +
      scale_fill_manual(
        values = c(
          Asia = "Yellow",
          Africa = "Brown",
          Americas = "Red",
          Europe = "Orange",
          Oceania = "Blue"
        )
      ) +
      scale_x_continuous(labels = unit_format(unit = "k", scale = 1e-3)) +
      theme(plot.title = element_text(
        hjust = 0.5,
        size = 15,
        face = 'bold'
      )) +
      geom_bar(stat = "identity") + 
      geom_text(
        aes(label = paste0(
          as.character(gdp_pc / 1000), "k"
        )),
        vjust = 0.4,
        color = "black",
        size = 3.8,
        hjust = -0.05
      ) +
      
      theme_bw() +
      theme(
        #legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12,
                                  face = "bold"),
        plot.title = element_text(size = 18,
                                  face = "bold"),
        plot.subtitle = element_text(size = 12),
        legend.title = element_text(size = 14,
                                    face = "bold"),
        legend.text = element_text(size = 12)
      )
    
    
  })
}

shinyApp(ui, server)
