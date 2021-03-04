setwd("../Data/Matched Data")
library(tidyverse)
library(lubridate)
library(plotly)
library(ggplot2)
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

figure1 = Ave_Summary %>%
  ggplot(aes(x=as.Date(fiscal_quarter), y = Ave_NewDeath, by=Country)) +
  geom_line(aes(color=Country)) +
  theme(legend.position ='none')
figure1

figure2 = Ave_Summary %>%
  ggplot(aes(x=as.Date(fiscal_quarter), y = Ave_NewCase, by=Country)) +
  geom_line(aes(color=Country)) +
  theme(legend.position ='none')
figure2

figure3 = Ave_Summary %>%
  ggplot(aes(x=as.Date(fiscal_quarter), y = CasePercent_Change, by=Country)) +
  geom_line(aes(color=Country)) +
  theme(legend.position ='none')
figure3

figure4 = Ave_Summary %>%
  ggplot(aes(x=as.Date(fiscal_quarter), y = DeathPercent_Change, by=Country)) +
  geom_line(aes(color=Country)) +
  theme(legend.position ='none')
figure4


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

figure5 = Cumul_Summary %>%
  ggplot(aes(x=as.Date(fiscal_quarter), y = Cumul_Case, by=Country)) +
  geom_line(aes(color=Country)) +
  theme(legend.position ='none')
figure5

figure6 = Cumul_Summary %>%
  ggplot(aes(x=as.Date(fiscal_quarter), y = Cumul_Death, by=Country)) +
  geom_line(aes(color=Country)) +
  theme(legend.position ='none')
figure6

figure7 = Cumul_Summary %>%
  ggplot(aes(x=as.Date(fiscal_quarter), y = CasePercent_Change, by=Country)) +
  geom_line(aes(color=Country)) +
  theme(legend.position ='none')
figure7

figure8 = Cumul_Summary %>%
  ggplot(aes(x=as.Date(fiscal_quarter), y = DeathPercent_Change, by=Country)) +
  geom_line(aes(color=Country)) +
  theme(legend.position ='none')
figure8


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


#Boxplots
ggplot(Ave_Summary, aes(x = fiscal_quarter, y = Ave_NewCase)) + geom_boxplot()
ggplot(Ave_Summary, aes(x = fiscal_quarter, y = Ave_NewDeath)) + geom_boxplot()
ggplot(Cumul_Summary, aes(x = fiscal_quarter, y = Cumul_Case)) + geom_boxplot()
ggplot(Cumul_Summary, aes(x = fiscal_quarter, y = Cumul_Death)) + geom_boxplot()
