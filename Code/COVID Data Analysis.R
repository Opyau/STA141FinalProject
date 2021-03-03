setwd("../Data/Matched Data")
library(tidyverse)
library(lubridate)
library(plotly)
covid <- read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv")
Country_GDP = read_csv("OECD Populations matched.csv")
countries = Country_GDP$Country


covid = covid %>% filter(Date_reported <= as.Date("2020-12-31")) %>%
  filter(Country %in% countries)

covid$fiscal_quarter = quarter(covid$Date_reported, with_year = TRUE, fiscal_start = 1)

for(i in c(1: dim(covid)[1])){
  if(covid$fiscal_quarter[i] == 2020.1)
    covid$fiscal_quarter[i] = "2020-01-03"
  else if(covid$fiscal_quarter[i] == 2020.2)
    covid$fiscal_quarter[i] = "2020-04-03"
  else if(covid$fiscal_quarter[i] == 2020.3)
    covid$fiscal_quarter[i] = "2020-07-03"
  else if(covid$fiscal_quarter[i] == 2020.4)
    covid$fiscal_quarter[i] = "2020-10-03"
  
}


Ave_Summary = covid %>% group_by(Country, fiscal_quarter) %>%
  summarize(Ave_NewCase = mean(New_cases),
            Ave_NewDeath = mean(New_deaths),
  ) 

Ave_Summary$CasePercent_Change = NA
Ave_Summary$DeathPercent_Change = NA

for(i in c(0:50)){
 for(j in c(1: 3)){
  Ave_Summary$CasePercent_Change[4*i+j+1] = abs(Ave_Summary$Ave_NewCase[4*i+j] - Ave_Summary$Ave_NewCase[4*i+j + 1])/Ave_Summary$Ave_NewCase[4*i+j]
  Ave_Summary$DeathPercent_Change[4*i+j+1] = abs(Ave_Summary$Ave_NewDeath[4*i+j] - Ave_Summary$Ave_NewDeath[4*i+j + 1])/Ave_Summary$Ave_NewDeath[4*i+j]
  }
}



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


Cumul_Summary = covid %>% group_by(Country, fiscal_quarter) %>%
  summarize(Cumul_Case = sum(New_cases),
            Cumul_Death = sum(New_deaths)
  )


Cumul_Summary$CasePercent_Change = NA
Cumul_Summary$DeathPercent_Change = NA

for(i in c(0:50)){
  for(j in c(1: 3)){
    Cumul_Summary$CasePercent_Change[4*i+j+1] = abs(Cumul_Summary$Cumul_Case[4*i+j] - Cumul_Summary$Cumul_Case[4*i+j + 1])/Cumul_Summary$Cumul_Case[4*i+j]
    Cumul_Summary$DeathPercent_Change[4*i+j+1] = abs(Cumul_Summary$Cumul_Death[4*i+j] - Cumul_Summary$Cumul_Death[4*i+j + 1])/Cumul_Summary$Cumul_Death[4*i+j]
  }
}

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

