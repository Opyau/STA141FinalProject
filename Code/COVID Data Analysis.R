setwd("../Data/Matched Data")

library(tidyverse)
library(lubridate)
library(expss)
library(plotly)
covid <- read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv")
Country_GDP = read_csv("OECD Populations matched.csv")
countries = Country_GDP$Country


covid = covid %>% filter(Date_reported <= as.Date("2020-12-31")) %>%
  filter(Country %in% countries)

covid$fiscal_quarter = quarter(covid$Date_reported, with_year = TRUE, fiscal_start = 1)


Ave_Summary = covid %>% group_by(Country, fiscal_quarter, Country_code) %>%
  summarize(Ave_NewCase = mean(New_cases),
            Ave_NewDeath = mean(New_deaths),
  ) 

Ave_Summary %>% plot_ly(
  x= ~Ave_NewCase,
  y= ~Ave_NewDeath,
  frame = ~fiscal_quarter,
  text=~Country,
  hoverinfo="Country",
  color=~Country,
  type = 'scatter',
  mode = 'markers',
  showlegend = T
) 


Cumul_Summary = covid %>% group_by(Country, fiscal_quarter, Country_code) %>%
  summarize(Cumul_Case = sum(New_cases),
            Cumul_Death = sum(New_deaths)
  )

Cumul_Summary %>%
  plot_ly(
    x= ~Cumul_Case,
    y= ~Cumul_Death,
    frame = ~fiscal_quarter,
    text=~Country,
    hoverinfo="Country",
    color=~Country,
    type = 'scatter',
    mode = 'markers',
    showlegend = T
  ) 


